;; title: bitcoin-native-prediction-market
;; Define constants
(define-constant contract-owner tx-sender)
(define-constant fee-percentage u5) ;; 0.5% fee
(define-constant admin-fee-percentage u1) ;; 0.1% admin fee
(define-constant resolution-delay u144) ;; ~1 day of Bitcoin blocks for dispute period
(define-constant oracle-consensus-threshold u80) ;; 80% consensus required
(define-constant min-stake u1000000) ;; Minimum amount to participate as oracle
(define-constant min-market-duration u720) ;; Minimum 5 days of market duration (in Bitcoin blocks)
(define-constant max-market-duration u52560) ;; Maximum ~1 year market duration
(define-constant error-unauthorized (err u1))
(define-constant error-invalid-market (err u2))
(define-constant error-invalid-state (err u3))
(define-constant error-invalid-amount (err u4))
(define-constant error-invalid-outcome (err u5))
(define-constant error-invalid-params (err u6))
(define-constant error-deadline-passed (err u7))
(define-constant error-deadline-not-reached (err u8))
(define-constant error-oracle-exists (err u9))
(define-constant error-not-oracle (err u10))
(define-constant error-already-voted (err u11))
(define-constant error-already-claimed (err u12))
(define-constant error-dispute-period (err u13))
(define-constant error-market-closed (err u14))
(define-constant error-not-finalized (err u15))

;; Define market state enum
(define-data-var market-id-nonce uint u0)


;; Data structures
(define-map markets
  uint
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-utf8 500),
    outcome-type: (buff 1),
    possible-outcomes: (list 10 (string-ascii 50)),
    creation-block: uint,
    resolution-block: uint,
    status: (buff 1),
    resolved-outcome: (optional (string-ascii 50)),
    resolution-timestamp: (optional uint),
    oracle-address: principal,
    total-liquidity: uint,
    oracle-fee: uint,
    dispute-status: (buff 1),
    dispute-resolution-block: (optional uint),
    market-fee: uint,
    outcome-values: (optional (list 10 uint)), ;; For scalar markets
    metadata: (optional (string-utf8 500))
  })

    ;; Liquidity pools for each outcome
(define-map liquidity-pools
  { market-id: uint, outcome: (string-ascii 50) }
  { amount: uint })

;; User positions in markets
(define-map positions
  { market-id: uint, user: principal, outcome: (string-ascii 50) }
  { amount: uint, claimed: bool })

;; Oracles registry
(define-map oracles
  principal
  {
    stake: uint,
    status: (buff 1),
    reliability-score: uint,
    markets-resolved: uint,
    total-disputes: uint,
    reputation: uint,
    registration-block: uint
  })

;; Oracle votes for market outcomes
(define-map oracle-votes
  { market-id: uint, oracle: principal }
  { outcome: (string-ascii 50), confidence: uint, timestamp: uint })

;; Disputes for market resolutions
(define-map disputes
  uint
  {
    disputer: principal,
    original-outcome: (string-ascii 50),
    proposed-outcome: (string-ascii 50),
    evidence: (string-utf8 500),
    stake: uint,
    resolution-votes: (list 100 principal),
    resolution-block: (optional uint),
    status: (buff 1)
  })

;; Market analytics
(define-map market-analytics
  uint
  {
    total-volume: uint,
    unique-participants: uint,
    last-trade-block: uint,
    largest-position: uint,
    price-history: (list 100 { block: uint, outcome: (string-ascii 50), price: uint })
  })

;; Update position data
(define-private (update-position (market-id uint) (user principal) (outcome (string-ascii 50)) (amount uint))
  (let ((position (default-to { amount: u0, claimed: false } 
                   (map-get? positions { market-id: market-id, user: user, outcome: outcome }))))
    (map-set positions
      { market-id: market-id, user: user, outcome: outcome }
      { amount: (+ (get amount position) amount), claimed: (get claimed position) })))

;; Increase oracle stake
(define-public (increase-oracle-stake (additional-stake uint))
  (let ((oracle (unwrap! (map-get? oracles tx-sender) error-not-oracle)))
    ;; Transfer additional stake to contract
    (try! (stx-transfer? additional-stake tx-sender (as-contract tx-sender)))
    
    ;; Update oracle stake
    (map-set oracles tx-sender
      (merge oracle { stake: (+ (get stake oracle) additional-stake) }))
    
    (ok true)))

;; Helper functions for enum conversions

(define-private (get-market-status-buff (status (buff 1)))
  status)

(define-private (get-outcome-type-buff (type (buff 1)))
  type)

(define-private (get-oracle-status-buff (status (buff 1)))
  status)

(define-private (get-dispute-status-buff (status (buff 1)))
  status)

;; Get market details
(define-read-only (get-market (market-id uint))
  (map-get? markets market-id))

;; Get market outcome pool
(define-read-only (get-outcome-pool (market-id uint) (outcome (string-ascii 50)))
  (map-get? liquidity-pools { market-id: market-id, outcome: outcome }))

;; Get user position
(define-read-only (get-user-position (market-id uint) (user principal) (outcome (string-ascii 50)))
  (map-get? positions { market-id: market-id, user: user, outcome: outcome }))

;; Get oracle details
(define-read-only (get-oracle (oracle-address principal))
  (map-get? oracles oracle-address))

;; Get market odds for an outcome
(define-read-only (get-market-odds (market-id uint) (outcome (string-ascii 50)))
  (let ((market (map-get? markets market-id))
        (pool (map-get? liquidity-pools { market-id: market-id, outcome: outcome })))
    (match market
      market-data (match pool
                    pool-data (let ((outcome-amount (get amount pool-data))
                                   (total-liquidity (get total-liquidity market-data)))
                                (if (> total-liquidity u0)
                                  (ok (/ (* outcome-amount u1000) total-liquidity))
                                  (ok u0)))
                    (err error-invalid-market))
      (err error-invalid-market))))

;; Get dispute details
(define-read-only (get-dispute (market-id uint))
  (map-get? disputes market-id))

;; Get market analytics
(define-read-only (get-market-analytics (market-id uint))
  (map-get? market-analytics market-id))


;; Calculate potential winnings
(define-read-only (calculate-potential-winnings (market-id uint) (outcome (string-ascii 50)) (amount uint))
  (let ((market (map-get? markets market-id))
        (pool (map-get? liquidity-pools { market-id: market-id, outcome: outcome })))
    (match market
      market-data (match pool
                    pool-data (let ((outcome-amount (get amount pool-data))
                                   (total-liquidity (get total-liquidity market-data))
                                   (market-fee-amount (/ (* total-liquidity (get market-fee market-data)) u1000))
                                   (oracle-fee-amount (/ (* total-liquidity (get oracle-fee market-data)) u1000))
                                   (winnings-pool (- total-liquidity (+ market-fee-amount oracle-fee-amount))))
                                (if (> outcome-amount u0)
                                  (ok (/ (* winnings-pool amount) outcome-amount))
                                  (ok u0)))
                    (err error-invalid-market))
      (err error-invalid-market))))


;; Helper function to calculate positions value
(define-private (calculate-positions-value (outcome (string-ascii 50)) (acc { user: principal, market-id: uint, total-value: uint }))
  (let ((position (default-to { amount: u0, claimed: false } 
                   (map-get? positions { market-id: (get market-id acc), user: (get user acc), outcome: outcome }))))
    { 
      user: (get user acc),
      market-id: (get market-id acc),
      total-value: (+ (get total-value acc) (get amount position))
    }))

(define-data-var protocol-paused bool false)

(define-public (pause-protocol)
  (begin
    (asserts! (is-eq tx-sender contract-owner) error-unauthorized)
    (var-set protocol-paused true)
    (ok true)))

(define-public (unpause-protocol)
  (begin
    (asserts! (is-eq tx-sender contract-owner) error-unauthorized)
    (var-set protocol-paused false)
    (ok true)))

(define-data-var protocol-fee-collector principal contract-owner)

(define-public (set-fee-collector (new-collector principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) error-unauthorized)
    (var-set protocol-fee-collector new-collector)
    (ok true)))

(define-data-var max-liquidity-per-market uint u1000000000000) ;; 1 million STX max per market

(define-public (set-max-liquidity (max-amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) error-unauthorized)
    (var-set max-liquidity-per-market max-amount)
    (ok true)))

(define-public (withdraw-oracle-stake (amount uint))
  (let ((oracle (unwrap! (map-get? oracles tx-sender) error-not-oracle))
        (current-stake (get stake oracle))
        (remaining-stake (- current-stake amount)))
    
    ;; Ensure minimum stake remains
    (asserts! (>= remaining-stake min-stake) error-invalid-amount)
    
    ;; Transfer stake back to oracle
    (as-contract (try! (stx-transfer? amount tx-sender tx-sender)))
    
    ;; Update oracle stake
    (map-set oracles tx-sender
      (merge oracle { stake: remaining-stake }))
    
    (ok true)))

(define-map market-oracles
  { market-id: uint, oracle: principal }
  { added-by: principal, weight: uint })

(define-map liquidity-providers
  { market-id: uint, provider: principal, outcome: (string-ascii 50) }
  { amount: uint, block-added: uint })

(define-map user-activity
  principal
  {
    markets-participated: uint,
    total-volume: uint,
    last-activity-block: uint,
    positions-count: uint,
    wins: uint,
    losses: uint
  })

(define-private (update-user-activity (user principal) (amount uint))
  (let ((activity (default-to {
                    markets-participated: u0,
                    total-volume: u0,
                    last-activity-block: stacks-block-height,
                    positions-count: u0,
                    wins: u0,
                    losses: u0
                  } (map-get? user-activity user))))
    (map-set user-activity user
      (merge activity {
        total-volume: (+ (get total-volume activity) amount),
        last-activity-block: stacks-block-height
      }))))

(define-map featured-markets
  uint
  { featured-until: uint, promoted-by: principal })

(define-map market-whitelist
  { market-id: uint, user: principal }
  { allowed: bool })

(define-data-var total-markets-created uint u0)
(define-data-var total-volume uint u0)
(define-data-var total-fees-collected uint u0)

(define-constant error-pool-liquidity (err u16))
(define-constant error-max-liquidity (err u17))
(define-constant error-paused (err u18))
(define-constant error-invalid-withdrawal (err u19))
(define-constant error-liquidity-locked (err u20))
(define-constant error-threshold-not-met (err u21))
(define-constant error-invalid-oracle (err u22))
(define-constant error-invalid-fee (err u23))
(define-constant error-invalid-category (err u24))

;; Example from add-market-liquidity
(asserts! (not (var-get protocol-paused)) error-paused)

;; Define market categories
(define-map market-categories
  uint
  {
    category: (string-ascii 20),
    subcategory: (optional (string-ascii 30))
  })

;; Mapping of categories to market-ids
(define-map category-markets
  (string-ascii 20)
  (list 100 uint))

;; Get markets by category
(define-read-only (get-markets-by-category (category (string-ascii 20)))
  (default-to (list) (map-get? category-markets category)))

;; Check if category is valid
(define-read-only (is-valid-category (category (string-ascii 20)))
  (or 
    (is-eq category "sports")
    (is-eq category "crypto")
    (is-eq category "politics")
    (is-eq category "finance")
    (is-eq category "entertainment")
    (is-eq category "science")
    (is-eq category "other")))

;; Get market category
(define-read-only (get-market-category (market-id uint))
  (map-get? market-categories market-id))

;; Helper to check if sender is market creator or contract owner
(define-private (is-market-creator-or-owner (market-id uint))
  (let ((market (map-get? markets market-id)))
    (match market
      market-data (or (is-eq tx-sender (get creator market-data))
                      (is-eq tx-sender contract-owner))
      false)))

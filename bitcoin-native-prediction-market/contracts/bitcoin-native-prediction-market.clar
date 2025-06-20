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




;; title: LogisticsChain
;; version: 1.0.0
;; summary: Supply chain optimization platform
;; description: Smart contract for managing supply chain logistics with real-time tracking, quality assurance, and automated settlements

;; traits
(define-trait subscription-trait
  (
    (get-subscription-status (principal) (response bool uint))
    (subscribe (uint) (response bool uint))
    (unsubscribe () (response bool uint))
  )
)

;; token definitions
;; None needed for basic functionality

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-not-subscriber (err u104))
(define-constant err-payment-failed (err u105))

;; data vars
(define-data-var next-shipment-id uint u1)
(define-data-var next-company-id uint u1)
(define-data-var subscription-fee uint u100) ;; in STX

(define-constant err-multisig-exists (err u130))
(define-constant err-multisig-not-found (err u131))
(define-constant err-insufficient-approvals (err u132))
(define-constant err-already-approved (err u133))
(define-constant err-not-authorized-approver (err u134))
(define-constant err-multisig-required (err u135))
(define-constant err-sla-exists (err u140))
(define-constant err-sla-not-found (err u141))
(define-constant err-sla-already-evaluated (err u142))
(define-constant err-invalid-sla-terms (err u143))
(define-constant err-sla-evaluation-too-early (err u144))

;; Dynamic Pricing Engine constants
(define-constant err-invalid-pricing-params (err u150))
(define-constant err-route-not-found (err u151))
(define-constant err-pricing-config-exists (err u152))
(define-constant err-insufficient-historical-data (err u153))
(define-constant err-invalid-cargo-type (err u154))

(define-map multisig-configs
  { shipment-id: uint }
  {
    required-approvals: uint,
    authorized-approvers: (list 5 principal),
    current-approvals: uint,
    approvers: (list 5 principal),
    operation-type: (string-ascii 30),
    operation-data: (string-ascii 100),
    status: (string-ascii 20),
    created-at: uint
  }
)

(define-map multisig-approvals
  { shipment-id: uint, approver: principal }
  {
    approved: bool,
    timestamp: uint,
    signature-hash: (buff 32)
  }
)

(define-map shipment-multisig-requirements
  { shipment-id: uint }
  {
    requires-multisig: bool,
    min-approvals: uint,
    authorized-parties: (list 5 principal)
  }
)

;; data maps
(define-map companies
  { company-id: uint }
  {
    name: (string-ascii 100),
    owner: principal,
    subscription-active: bool,
    subscription-expiry: uint
  }
)

(define-map company-principals
  { principal: principal }
  { company-id: uint }
)

(define-map shipments
  { shipment-id: uint }
  {
    origin: uint,  ;; company-id
    destination: uint,  ;; company-id
    product: (string-ascii 100),
    quantity: uint,
    created-at: uint,
    status: (string-ascii 20),  ;; "created", "in-transit", "delivered", "rejected"
    quality-score: (optional uint),
    last-updated: uint,
    settlement-complete: bool
  }
)

(define-map shipment-tracking
  { shipment-id: uint, timestamp: uint }
  {
    location: (string-ascii 100),
    temperature: int,
    humidity: uint,
    notes: (string-ascii 255)
  }
)

(define-map quality-requirements
  { product: (string-ascii 100) }
  {
    min-temperature: int,
    max-temperature: int,
    min-humidity: uint,
    max-humidity: uint
  }
)

;; public functions
(define-public (register-company (name (string-ascii 100)))
  (let
    (
      (company-id (var-get next-company-id))
    )
    (asserts! (is-none (map-get? company-principals { principal: tx-sender })) err-already-exists)
    
    (map-set companies
      { company-id: company-id }
      {
        name: name,
        owner: tx-sender,
        subscription-active: false,
        subscription-expiry: u0
      }
    )
    
    (map-set company-principals
      { principal: tx-sender }
      { company-id: company-id }
    )
    
    (var-set next-company-id (+ company-id u1))
    (ok company-id)
  )
)

(define-public (subscribe (duration uint))
  (let
    (
      (company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
      (company-id (get company-id company-data))
      (fee (* (var-get subscription-fee) duration))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
      (company (unwrap! (map-get? companies { company-id: company-id }) err-not-found))
      (new-expiry (if (> (get subscription-expiry company) current-time)
                     (+ (get subscription-expiry company) (* duration u2592000)) ;; duration in months (30 days in seconds)
                     (+ current-time (* duration u2592000))))
    )
    
    ;; Transfer subscription fee to contract
    (unwrap! (stx-transfer? fee tx-sender (as-contract tx-sender)) err-payment-failed)
    
    ;; Update subscription status
    (map-set companies
      { company-id: company-id }
      (merge company {
        subscription-active: true,
        subscription-expiry: new-expiry
      })
    )
    
    (ok true)
  )
)

(define-public (create-shipment (destination-id uint) (product (string-ascii 100)) (quantity uint))
  (let
    (
      (company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
      (origin-id (get company-id company-data))
      (shipment-id (var-get next-shipment-id))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))

    )
    
    ;; Check if company has active subscription
    (asserts! (is-subscription-active origin-id) err-not-subscriber)
    (asserts! (is-subscription-active destination-id) err-not-subscriber)
    
    (map-set shipments
      { shipment-id: shipment-id }
      {
        origin: origin-id,
        destination: destination-id,
        product: product,
        quantity: quantity,
        created-at: current-time,
        status: "created",
        quality-score: none,
        last-updated: current-time,
        settlement-complete: false
      }
    )
    
    (var-set next-shipment-id (+ shipment-id u1))
    (ok shipment-id)
  )
)

(define-public (update-shipment-status (shipment-id uint) (new-status (string-ascii 20)))
  (let
    (
      (company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
      (company-id (get company-id company-data))
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) err-not-found))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    
    ;; Check if company has active subscription
    (asserts! (is-subscription-active company-id) err-not-subscriber)
    
    ;; Check if company is authorized to update this shipment
    (asserts! (or (is-eq (get origin shipment) company-id) (is-eq (get destination shipment) company-id)) err-not-authorized)
    
    ;; Check if new status is valid
    (asserts! (or (is-eq new-status "in-transit") (is-eq new-status "delivered") (is-eq new-status "rejected")) err-invalid-status)
    
    ;; Update shipment status
    (map-set shipments
      { shipment-id: shipment-id }
      (merge shipment {
        status: new-status,
        last-updated: current-time
      })
    )
    
    ;; If delivered or rejected, trigger settlement
    (if (or (is-eq new-status "delivered") (is-eq new-status "rejected"))
        (settle-shipment shipment-id)
        (ok true))
  )
)

(define-public (add-tracking-data (shipment-id uint) (location (string-ascii 100)) (temperature int) (humidity uint) (notes (string-ascii 255)))
  (let
    (
      (company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
      (company-id (get company-id company-data))
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) err-not-found))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    
    ;; Check if company has active subscription
    (asserts! (is-subscription-active company-id) err-not-subscriber)
    
    ;; Check if company is authorized to update this shipment
    (asserts! (or (is-eq (get origin shipment) company-id) (is-eq (get destination shipment) company-id)) err-not-authorized)
    
    ;; Add tracking data
    (map-set shipment-tracking
      { shipment-id: shipment-id, timestamp: current-time }
      {
        location: location,
        temperature: temperature,
        humidity: humidity,
        notes: notes
      }
    )
    
    (ok true)
  )
)

(define-public (set-quality-requirements (product (string-ascii 100)) (min-temp int) (max-temp int) (min-humidity uint) (max-humidity uint))
  (let
    (
      (company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
      (company-id (get company-id company-data))
    )
    
    ;; Check if company has active subscription
    (asserts! (is-subscription-active company-id) err-not-subscriber)
    
    (map-set quality-requirements
      { product: product }
      {
        min-temperature: min-temp,
        max-temperature: max-temp,
        min-humidity: min-humidity,
        max-humidity: max-humidity
      }
    )
    
    (ok true)
  )
)

(define-public (assess-quality (shipment-id uint) (quality-score uint))
  (let
    (
      (company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
      (company-id (get company-id company-data))
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) err-not-found))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    
    ;; Check if company has active subscription
    (asserts! (is-subscription-active company-id) err-not-subscriber)
    
    ;; Check if company is the destination (receiver)
    (asserts! (is-eq (get destination shipment) company-id) err-not-authorized)
    
    ;; Check if quality score is valid (0-100)
    (asserts! (<= quality-score u100) (err u106))
    
    ;; Update shipment with quality score
    (map-set shipments
      { shipment-id: shipment-id }
      (merge shipment {
        quality-score: (some quality-score),
        last-updated: current-time
      })
    )
    
    (ok true)
  )
)

(define-public (settle-shipment (shipment-id uint))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) err-not-found))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    
    ;; Check if shipment is delivered or rejected
    (asserts! (or (is-eq (get status shipment) "delivered") (is-eq (get status shipment) "rejected")) err-invalid-status)
    
    ;; Check if settlement is not already complete
    (asserts! (not (get settlement-complete shipment)) (err u107))
    
    ;; Mark settlement as complete
    (map-set shipments
      { shipment-id: shipment-id }
      (merge shipment {
        settlement-complete: true,
        last-updated: current-time
      })
    )
    
    
    (ok true)
  )
)

(define-public (update-subscription-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (var-set subscription-fee new-fee)
    (ok true)
  )
)

;; read only functions
(define-read-only (get-shipment (shipment-id uint))
  (map-get? shipments { shipment-id: shipment-id })
)

(define-read-only (get-company (company-id uint))
  (map-get? companies { company-id: company-id })
)

(define-read-only (get-company-by-principal (principal principal))
  (map-get? company-principals { principal: principal })
)

(define-read-only (get-tracking-history (shipment-id uint) (timestamp uint))
  (map-get? shipment-tracking { shipment-id: shipment-id, timestamp: timestamp })
)

(define-read-only (get-quality-requirements (product (string-ascii 100)))
  (map-get? quality-requirements { product: product })
)

(define-read-only (get-subscription-fee)
  (var-get subscription-fee)
)

(define-read-only (is-subscription-active (company-id uint))
  (let
    (
      (company (unwrap! (map-get? companies { company-id: company-id }) false))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))

    )
    (and (get subscription-active company) (> (get subscription-expiry company) current-time))
  )
)

;; private functions
(define-private (is-authorized-for-shipment (shipment-id uint) (company-id uint))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) false))
    )
    (or (is-eq (get origin shipment) company-id) (is-eq (get destination shipment) company-id))
  )
)



(define-data-var max-batch-size uint u10)

(define-public (create-batch-shipments 
    (destination-ids (list 10 uint))
    (products (list 10 (string-ascii 100)))
    (quantities (list 10 uint)))
    (let
        ((company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
         (origin-id (get company-id company-data)))
        (asserts! (<= (len destination-ids) (var-get max-batch-size)) (err u108))
        (asserts! (is-eq (len destination-ids) (len products)) (err u109))
        (asserts! (is-eq (len products) (len quantities)) (err u109))
        (ok (map create-single-batch-shipment destination-ids products quantities))
    )
)

(define-private (create-single-batch-shipment 
    (destination-id uint)
    (product (string-ascii 100))
    (quantity uint))
    (create-shipment destination-id product quantity)
)



(define-public (get-company-subscription-status (company-id uint))
  (let
    (
      (company (unwrap! (map-get? companies { company-id: company-id }) err-not-found))
    )
    (ok {
      subscription-active: (get subscription-active company),
      subscription-expiry: (get subscription-expiry company)
    })
  )
)


(define-map shipment-ratings
    { shipment-id: uint }
    {
        rating: uint,
        feedback: (string-ascii 500),
        rated-by: principal
    }
)

(define-public (rate-shipment 
    (shipment-id uint)
    (rating uint)
    (feedback (string-ascii 500)))
    (let
        ((shipment (unwrap! (get-shipment shipment-id) err-not-found))
         (company-data (unwrap! (get-company-by-principal tx-sender) err-not-found)))
        (asserts! (<= rating u5) (err u110))
        (asserts! (is-eq (get destination shipment) (get company-id company-data)) err-not-authorized)
        (asserts! (is-eq (get status shipment) "delivered") err-invalid-status)
        (ok (map-set shipment-ratings
            { shipment-id: shipment-id }
            {
                rating: rating,
                feedback: feedback,
                rated-by: tx-sender
            }
        ))
    )
)


(define-map shipment-priorities
    { shipment-id: uint }
    { 
        priority-level: (string-ascii 20),
        extra-fee: uint
    }
)

(define-public (set-shipment-priority
    (shipment-id uint)
    (priority-level (string-ascii 20)))
    (let
        ((shipment (unwrap! (get-shipment shipment-id) err-not-found))
         (fee (get-priority-fee priority-level)))
        (asserts! (or (is-eq priority-level "standard")
                     (is-eq priority-level "express")
                     (is-eq priority-level "urgent")) (err u111))
        (unwrap! (stx-transfer? fee tx-sender (as-contract tx-sender)) err-payment-failed)
        (ok (map-set shipment-priorities
            { shipment-id: shipment-id }
            {
                priority-level: priority-level,
                extra-fee: fee
            }
        ))
    )
)

(define-private (get-priority-fee (priority-level (string-ascii 20)))
    (if (is-eq priority-level "express")
        u50
        (if (is-eq priority-level "urgent")
            u100
            u0)
    )
)


(define-map shipment-insurance
    { shipment-id: uint }
    {
        coverage-amount: uint,
        premium-paid: uint,
        insured-by: principal
    }
)

(define-public (insure-shipment 
    (shipment-id uint)
    (coverage-amount uint))
    (let
        ((premium (calculate-premium coverage-amount))
         (shipment (unwrap! (get-shipment shipment-id) err-not-found)))
        (asserts! (is-eq (get status shipment) "created") err-invalid-status)
        (unwrap! (stx-transfer? premium tx-sender (as-contract tx-sender)) err-payment-failed)
        (ok (map-set shipment-insurance
            { shipment-id: shipment-id }
            {
                coverage-amount: coverage-amount,
                premium-paid: premium,
                insured-by: tx-sender
            }
        ))
    )
)

(define-private (calculate-premium (coverage-amount uint))
    (/ (* coverage-amount u3) u100)
)

(define-map shipment-documents
    { shipment-id: uint, document-type: (string-ascii 20) }
    {
        hash: (buff 32),
        uploaded-by: principal,
        timestamp: uint
    }
)

(define-public (add-shipment-document 
    (shipment-id uint)
    (document-type (string-ascii 20))
    (document-hash (buff 32)))
    (let
        ((company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
         (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1)))))
        (asserts! (is-authorized-for-shipment shipment-id (get company-id company-data)) err-not-authorized)
        (ok (map-set shipment-documents
            { shipment-id: shipment-id, document-type: document-type }
            {
                hash: document-hash,
                uploaded-by: tx-sender,
                timestamp: current-time
            }
        ))
    )
)


(define-map shipment-routes
    { shipment-id: uint }
    {
        stops: (list 5 uint),
        current-stop: uint,
        route-complete: bool
    }
)

(define-public (create-route
    (shipment-id uint)
    (stop-locations (list 5 uint)))
    (let
        ((shipment (unwrap! (get-shipment shipment-id) err-not-found))
         (company-data (unwrap! (get-company-by-principal tx-sender) err-not-found)))
        (asserts! (is-eq (get origin shipment) (get company-id company-data)) err-not-authorized)
        (asserts! (> (len stop-locations) u0) (err u112))
        (ok (map-set shipment-routes
            { shipment-id: shipment-id }
            {
                stops: stop-locations,
                current-stop: u0,
                route-complete: false
            }
        ))
    )
)

(define-public (update-route-progress
    (shipment-id uint))
    (let
        ((route (unwrap! (map-get? shipment-routes { shipment-id: shipment-id }) err-not-found))
         (company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
         (next-stop (+ (get current-stop route) u1)))
        (asserts! (is-authorized-for-shipment shipment-id (get company-id company-data)) err-not-authorized)
        (asserts! (< next-stop (len (get stops route))) (err u113))
        (ok (map-set shipment-routes
            { shipment-id: shipment-id }
            (merge route {
                current-stop: next-stop,
                route-complete: (is-eq next-stop (- (len (get stops route)) u1))
            })
        ))
    )
)

(define-map shipment-disputes
    { shipment-id: uint }
    {
        reason: (string-ascii 500),
        filed-by: principal,
        status: (string-ascii 20),
        resolution: (optional (string-ascii 500)),
        timestamp: uint
    }
)

(define-public (file-dispute
    (shipment-id uint)
    (reason (string-ascii 500)))
    (let
        ((company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
         (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1)))))
        (asserts! (is-authorized-for-shipment shipment-id (get company-id company-data)) err-not-authorized)
        (ok (map-set shipment-disputes
            { shipment-id: shipment-id }
            {
                reason: reason,
                filed-by: tx-sender,
                status: "pending",
                resolution: none,
                timestamp: current-time
            }
        ))
    )
)



;; Analytics data structures
(define-map company-analytics
  { company-id: uint }
  {
    total-shipments: uint,
    completed-shipments: uint,
    rejected-shipments: uint,
    total-quality-score: uint,
    avg-delivery-time: uint
  }
)

(define-map global-analytics
  { key: (string-ascii 20) }
  {
    total-shipments: uint,
    completed-shipments: uint,
    rejected-shipments: uint,
    avg-quality-score: uint
  }
)

;; Initialize global analytics
(map-set global-analytics
  { key: "stats" }
  {
    total-shipments: u0,
    completed-shipments: u0,
    rejected-shipments: u0,
    avg-quality-score: u0
  }
)

;; Update analytics when shipment is created
(define-private (update-analytics-shipment-created (origin-id uint) (destination-id uint))
  (let
    (
      (origin-analytics (default-to 
        { total-shipments: u0, completed-shipments: u0, rejected-shipments: u0, total-quality-score: u0, avg-delivery-time: u0 } 
        (map-get? company-analytics { company-id: origin-id })))
      (destination-analytics (default-to 
        { total-shipments: u0, completed-shipments: u0, rejected-shipments: u0, total-quality-score: u0, avg-delivery-time: u0 } 
        (map-get? company-analytics { company-id: destination-id })))
      (global-stats (default-to 
        { total-shipments: u0, completed-shipments: u0, rejected-shipments: u0, avg-quality-score: u0 } 
        (map-get? global-analytics { key: "stats" })))
    )
    
    ;; Update origin company analytics
    (map-set company-analytics
      { company-id: origin-id }
      (merge origin-analytics {
        total-shipments: (+ (get total-shipments origin-analytics) u1)
      })
    )
    
    ;; Update destination company analytics
    (map-set company-analytics
      { company-id: destination-id }
      (merge destination-analytics {
        total-shipments: (+ (get total-shipments destination-analytics) u1)
      })
    )
    
    ;; Update global analytics
    (map-set global-analytics
      { key: "stats" }
      (merge global-stats {
        total-shipments: (+ (get total-shipments global-stats) u1)
      })
    )
  )
)

;; Update analytics when shipment status changes
(define-private (update-analytics-status-change (shipment-id uint) (new-status (string-ascii 20)))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) (ok false)))
      (origin-id (get origin shipment))
      (destination-id (get destination shipment))
      (quality-score (default-to u0 (get quality-score shipment)))
      (origin-analytics (default-to 
        { total-shipments: u0, completed-shipments: u0, rejected-shipments: u0, total-quality-score: u0, avg-delivery-time: u0 } 
        (map-get? company-analytics { company-id: origin-id })))
      (destination-analytics (default-to 
        { total-shipments: u0, completed-shipments: u0, rejected-shipments: u0, total-quality-score: u0, avg-delivery-time: u0 } 
        (map-get? company-analytics { company-id: destination-id })))
      (global-stats (default-to 
        { total-shipments: u0, completed-shipments: u0, rejected-shipments: u0, avg-quality-score: u0 } 
        (map-get? global-analytics { key: "stats" })))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
      (delivery-time (if (is-eq new-status "delivered") 
                        (- current-time (get created-at shipment))
                        u0))
    )
    
    (if (is-eq new-status "delivered")
      (begin
        ;; Update origin company analytics
        (map-set company-analytics
          { company-id: origin-id }
          (merge origin-analytics {
            completed-shipments: (+ (get completed-shipments origin-analytics) u1),
            total-quality-score: (+ (get total-quality-score origin-analytics) quality-score),
            avg-delivery-time: (if (is-eq (get completed-shipments origin-analytics) u0)
                                 delivery-time
                                 (/ (+ (* (get avg-delivery-time origin-analytics) (get completed-shipments origin-analytics)) delivery-time)
                                    (+ (get completed-shipments origin-analytics) u1)))
          })
        )
        
        ;; Update destination company analytics
        (map-set company-analytics
          { company-id: destination-id }
          (merge destination-analytics {
            completed-shipments: (+ (get completed-shipments destination-analytics) u1),
            total-quality-score: (+ (get total-quality-score destination-analytics) quality-score),
            avg-delivery-time: (if (is-eq (get completed-shipments destination-analytics) u0)
                                 delivery-time
                                 (/ (+ (* (get avg-delivery-time destination-analytics) (get completed-shipments destination-analytics)) delivery-time)
                                    (+ (get completed-shipments destination-analytics) u1)))
          })
        )
        
        ;; Update global analytics
        (map-set global-analytics
          { key: "stats" }
          (merge global-stats {
            completed-shipments: (+ (get completed-shipments global-stats) u1),
            avg-quality-score: (if (is-eq (get completed-shipments global-stats) u0)
                                quality-score
                                (/ (+ (* (get avg-quality-score global-stats) (get completed-shipments global-stats)) quality-score)
                                   (+ (get completed-shipments global-stats) u1)))
          })
        )
        (ok true)
      )
      (if (is-eq new-status "rejected")
        (begin
          ;; Update origin company analytics
          (map-set company-analytics
            { company-id: origin-id }
            (merge origin-analytics {
              rejected-shipments: (+ (get rejected-shipments origin-analytics) u1)
            })
          )
          
          ;; Update destination company analytics
          (map-set company-analytics
            { company-id: destination-id }
            (merge destination-analytics {
              rejected-shipments: (+ (get rejected-shipments destination-analytics) u1)
            })
          )
          
          ;; Update global analytics
          (map-set global-analytics
            { key: "stats" }
            (merge global-stats {
              rejected-shipments: (+ (get rejected-shipments global-stats) u1)
            })
          )
          (ok true)
        )
        (ok true)
      )
    )
  )
)

;; Modified create-shipment function to update analytics
(define-public (create-shipment-new (destination-id uint) (product (string-ascii 100)) (quantity uint))
  (let
    (
      (company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
      (origin-id (get company-id company-data))
      (shipment-id (var-get next-shipment-id))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    
    ;; Check if company has active subscription
    (asserts! (is-subscription-active origin-id) err-not-subscriber)
    (asserts! (is-subscription-active destination-id) err-not-subscriber)
    
    (map-set shipments
      { shipment-id: shipment-id }
      {
        origin: origin-id,
        destination: destination-id,
        product: product,
        quantity: quantity,
        created-at: current-time,
        status: "created",
        quality-score: none,
        last-updated: current-time,
        settlement-complete: false
      }
    )
    
    (var-set next-shipment-id (+ shipment-id u1))
    
    ;; Update analytics
    (update-analytics-shipment-created origin-id destination-id)
    
    (ok shipment-id)
  )
)


;; Read-only functions for analytics
(define-read-only (get-company-analytics (company-id uint))
  (map-get? company-analytics { company-id: company-id })
)

(define-read-only (get-global-analytics)
  (map-get? global-analytics { key: "stats" })
)

(define-read-only (get-company-performance-metrics (company-id uint))
  (let
    (
      (analytics (default-to 
        { total-shipments: u0, completed-shipments: u0, rejected-shipments: u0, total-quality-score: u0, avg-delivery-time: u0 } 
        (map-get? company-analytics { company-id: company-id })))
    )
    {
      on-time-delivery-rate: (if (is-eq (get total-shipments analytics) u0)
                               u0
                               (/ (* (get completed-shipments analytics) u100) (get total-shipments analytics))),
      avg-quality-score: (if (is-eq (get completed-shipments analytics) u0)
                           u0
                           (/ (get total-quality-score analytics) (get completed-shipments analytics))),
      avg-delivery-time: (get avg-delivery-time analytics)
    }
  )
)


;; Escrow system data structures
(define-map escrow-accounts
  { shipment-id: uint }
  {
    amount: uint,
    sender: principal,
    receiver: principal,
    release-threshold: uint,
    status: (string-ascii 20),
    created-at: uint
  }
)

(define-constant err-escrow-exists (err u120))
(define-constant err-escrow-not-found (err u121))
(define-constant err-insufficient-funds (err u122))
(define-constant err-unauthorized-escrow (err u123))
(define-constant err-invalid-escrow-status (err u124))
(define-constant err-quality-below-threshold (err u125))

;; Create an escrow for a shipment
(define-public (create-escrow (shipment-id uint) (amount uint) (release-threshold uint))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) err-not-found))
      (origin-company (unwrap! (map-get? companies { company-id: (get origin shipment) }) err-not-found))
      (destination-company (unwrap! (map-get? companies { company-id: (get destination shipment) }) err-not-found))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    
    ;; Check if escrow already exists
    (asserts! (is-none (map-get? escrow-accounts { shipment-id: shipment-id })) err-escrow-exists)
    
    ;; Check if sender is the destination company owner (buyer)
    (asserts! (is-eq tx-sender (get owner destination-company)) err-not-authorized)
    
    ;; Transfer funds to contract
    (unwrap! (stx-transfer? amount tx-sender (as-contract tx-sender)) err-insufficient-funds)
    
    ;; Create escrow account
    (map-set escrow-accounts
      { shipment-id: shipment-id }
      {
        amount: amount,
        sender: tx-sender,
        receiver: (get owner origin-company),
        release-threshold: release-threshold,
        status: "locked",
        created-at: current-time
      }
    )
    
    (ok true)
  )
)



(define-public (setup-multisig-shipment 
    (shipment-id uint) 
    (min-approvals uint) 
    (authorized-parties (list 5 principal)))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) err-not-found))
      (company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
      (company-id (get company-id company-data))
    )
    
    (asserts! (is-eq (get origin shipment) company-id) err-not-authorized)
    (asserts! (> min-approvals u0) (err u136))
    (asserts! (<= min-approvals (len authorized-parties)) (err u137))
    (asserts! (is-none (map-get? shipment-multisig-requirements { shipment-id: shipment-id })) err-multisig-exists)
    
    (map-set shipment-multisig-requirements
      { shipment-id: shipment-id }
      {
        requires-multisig: true,
        min-approvals: min-approvals,
        authorized-parties: authorized-parties
      }
    )
    
    (ok true)
  )
)

(define-public (create-multisig-operation 
    (shipment-id uint) 
    (operation-type (string-ascii 30)) 
    (operation-data (string-ascii 100)))
  (let
    (
      (multisig-req (unwrap! (map-get? shipment-multisig-requirements { shipment-id: shipment-id }) err-multisig-not-found))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    
    (asserts! (get requires-multisig multisig-req) err-multisig-required)
    (asserts! (is-authorized-multisig-user shipment-id tx-sender) err-not-authorized-approver)
    (asserts! (is-none (map-get? multisig-configs { shipment-id: shipment-id })) err-multisig-exists)
    
    (map-set multisig-configs
      { shipment-id: shipment-id }
      {
        required-approvals: (get min-approvals multisig-req),
        authorized-approvers: (get authorized-parties multisig-req),
        current-approvals: u0,
        approvers: (list),
        operation-type: operation-type,
        operation-data: operation-data,
        status: "pending",
        created-at: current-time
      }
    )
    
    (ok true)
  )
)

(define-public (approve-multisig-operation 
    (shipment-id uint) 
    (signature-hash (buff 32)))
  (let
    (
      (multisig-config (unwrap! (map-get? multisig-configs { shipment-id: shipment-id }) err-multisig-not-found))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
      (existing-approval (map-get? multisig-approvals { shipment-id: shipment-id, approver: tx-sender }))
    )
    
    (asserts! (is-authorized-multisig-user shipment-id tx-sender) err-not-authorized-approver)
    (asserts! (is-eq (get status multisig-config) "pending") err-invalid-escrow-status)
    (asserts! (is-none existing-approval) err-already-approved)
    
    (map-set multisig-approvals
      { shipment-id: shipment-id, approver: tx-sender }
      {
        approved: true,
        timestamp: current-time,
        signature-hash: signature-hash
      }
    )
    
    (let
      (
        (new-approval-count (+ (get current-approvals multisig-config) u1))
        (updated-approvers (unwrap! (as-max-len? (append (get approvers multisig-config) tx-sender) u5) (err u138)))
      )
      
      (map-set multisig-configs
        { shipment-id: shipment-id }
        (merge multisig-config {
          current-approvals: new-approval-count,
          approvers: updated-approvers,
          status: (if (>= new-approval-count (get required-approvals multisig-config)) "approved" "pending")
        })
      )
    )
    (ok true)
  )
)



(define-public (multisig-update-shipment-status 
    (shipment-id uint) 
    (new-status (string-ascii 20)))
  (let
    (
      (multisig-req (map-get? shipment-multisig-requirements { shipment-id: shipment-id }))
    )
    
    (if (is-some multisig-req)
        (begin
          (unwrap! (create-multisig-operation shipment-id "status-change" new-status) err-multisig-not-found)
          (ok true)
        )
        (update-shipment-status shipment-id new-status))
  )
)

(define-public (multisig-settle-shipment (shipment-id uint))
  (let
    (
      (multisig-req (map-get? shipment-multisig-requirements { shipment-id: shipment-id }))
    )
    
    (if (is-some multisig-req)
        (begin
          (unwrap! (create-multisig-operation shipment-id "settlement" "settle") err-multisig-not-found)
          (ok true)
        )
        (settle-shipment shipment-id))
  )
)

(define-public (revoke-multisig-approval (shipment-id uint))
  (let
    (
      (multisig-config (unwrap! (map-get? multisig-configs { shipment-id: shipment-id }) err-multisig-not-found))
      (existing-approval (unwrap! (map-get? multisig-approvals { shipment-id: shipment-id, approver: tx-sender }) err-not-found))
    )
    
    (asserts! (is-eq (get status multisig-config) "pending") err-invalid-escrow-status)
    (asserts! (get approved existing-approval) err-not-found)
    
    (map-delete multisig-approvals { shipment-id: shipment-id, approver: tx-sender })
    
    (let
      (
        (new-approval-count (- (get current-approvals multisig-config) u1))
        (filtered-approvers (filter-approver (get approvers multisig-config) tx-sender))
      )
      
      (map-set multisig-configs
        { shipment-id: shipment-id }
        (merge multisig-config {
          current-approvals: new-approval-count,
          approvers: filtered-approvers,
          status: "pending"
        })
      )
    )
    
    (ok true)
  )
)

(define-read-only (get-multisig-config (shipment-id uint))
  (map-get? multisig-configs { shipment-id: shipment-id })
)

(define-read-only (get-multisig-requirements (shipment-id uint))
  (map-get? shipment-multisig-requirements { shipment-id: shipment-id })
)

(define-read-only (get-multisig-approval (shipment-id uint) (approver principal))
  (map-get? multisig-approvals { shipment-id: shipment-id, approver: approver })
)

(define-read-only (is-multisig-ready (shipment-id uint))
  (let
    (
      (multisig-config (map-get? multisig-configs { shipment-id: shipment-id }))
    )
    
    (match multisig-config
      config (>= (get current-approvals config) (get required-approvals config))
      false)
  )
)

(define-read-only (get-multisig-status (shipment-id uint))
  (let
    (
      (multisig-config (map-get? multisig-configs { shipment-id: shipment-id }))
      (multisig-req (map-get? shipment-multisig-requirements { shipment-id: shipment-id }))
    )
    
    {
      has-multisig: (is-some multisig-req),
      pending-operation: (is-some multisig-config),
      ready-to-execute: (is-multisig-ready shipment-id),
      current-approvals: (match multisig-config config (get current-approvals config) u0),
      required-approvals: (match multisig-req req (get min-approvals req) u0)
    }
  )
)

(define-private (is-authorized-multisig-user (shipment-id uint) (user principal))
  (let
    (
      (multisig-req (unwrap! (map-get? shipment-multisig-requirements { shipment-id: shipment-id }) false))
    )
    
    (is-some (index-of (get authorized-parties multisig-req) user))
  )
)

(define-private (filter-approver (approvers (list 5 principal)) (to-remove principal))
  (filter is-not-target-approver approvers)
)

(define-private (is-not-target-approver (approver principal))
  (not (is-eq approver tx-sender))
)

(define-map sla-contracts
  { shipment-id: uint }
  {
    delivery-deadline: uint,
    quality-threshold: uint,
    penalty-amount: uint,
    reward-amount: uint,
    created-by: principal,
    status: (string-ascii 20),
    evaluated: bool,
    evaluation-result: (optional (string-ascii 20)),
    created-at: uint
  }
)

(define-public (create-sla (shipment-id uint) (delivery-deadline uint) (quality-threshold uint) (penalty-amount uint) (reward-amount uint))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) err-not-found))
      (company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
      (company-id (get company-id company-data))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    
    (asserts! (is-eq (get origin shipment) company-id) err-not-authorized)
    (asserts! (is-none (map-get? sla-contracts { shipment-id: shipment-id })) err-sla-exists)
    (asserts! (> delivery-deadline current-time) err-invalid-sla-terms)
    (asserts! (> quality-threshold u0) err-invalid-sla-terms)
    (asserts! (<= quality-threshold u100) err-invalid-sla-terms)
    (asserts! (> penalty-amount u0) err-invalid-sla-terms)
    (asserts! (> reward-amount u0) err-invalid-sla-terms)
    
    (unwrap! (stx-transfer? (+ penalty-amount reward-amount) tx-sender (as-contract tx-sender)) err-payment-failed)
    
    (map-set sla-contracts
      { shipment-id: shipment-id }
      {
        delivery-deadline: delivery-deadline,
        quality-threshold: quality-threshold,
        penalty-amount: penalty-amount,
        reward-amount: reward-amount,
        created-by: tx-sender,
        status: "active",
        evaluated: false,
        evaluation-result: none,
        created-at: current-time
      }
    )
    
    (ok true)
  )
)

(define-public (evaluate-sla (shipment-id uint))
  (let
    (
      (sla (unwrap! (map-get? sla-contracts { shipment-id: shipment-id }) err-sla-not-found))
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) err-not-found))
      (company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
      (company-id (get company-id company-data))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
      (destination-company (unwrap! (map-get? companies { company-id: (get destination shipment) }) err-not-found))
      (quality-score (default-to u0 (get quality-score shipment)))
      (is-delivered (is-eq (get status shipment) "delivered"))
      (is-rejected (is-eq (get status shipment) "rejected"))
      (is-on-time (and is-delivered (<= (get last-updated shipment) (get delivery-deadline sla))))
      (meets-quality (>= quality-score (get quality-threshold sla)))
      (exceeds-quality (>= quality-score (+ (get quality-threshold sla) u20)))
    )
    
    (asserts! (is-eq (get destination shipment) company-id) err-not-authorized)
    (asserts! (not (get evaluated sla)) err-sla-already-evaluated)
    (asserts! (or is-delivered is-rejected) err-sla-evaluation-too-early)
    
    (if (and is-delivered is-on-time meets-quality)
      (begin
        (if exceeds-quality
          (begin
            (unwrap! (as-contract (stx-transfer? (get reward-amount sla) tx-sender (get owner destination-company))) err-payment-failed)
            (map-set sla-contracts
              { shipment-id: shipment-id }
              (merge sla {
                evaluated: true,
                evaluation-result: (some "reward-paid"),
                status: "completed"
              })
            )
            (ok "reward-paid")
          )
          (begin
            (map-set sla-contracts
              { shipment-id: shipment-id }
              (merge sla {
                evaluated: true,
                evaluation-result: (some "terms-met"),
                status: "completed"
              })
            )
            (ok "terms-met")
          )
        )
      )
      (begin
        (unwrap! (as-contract (stx-transfer? (get penalty-amount sla) tx-sender (get owner destination-company))) err-payment-failed)
        (map-set sla-contracts
          { shipment-id: shipment-id }
          (merge sla {
            evaluated: true,
            evaluation-result: (some "penalty-paid"),
            status: "breached"
          })
        )
        (ok "penalty-paid")
      )
    )
  )
)

(define-public (cancel-sla (shipment-id uint))
  (let
    (
      (sla (unwrap! (map-get? sla-contracts { shipment-id: shipment-id }) err-sla-not-found))
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) err-not-found))
      (company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
      (company-id (get company-id company-data))
    )
    
    (asserts! (is-eq (get created-by sla) tx-sender) err-not-authorized)
    (asserts! (not (get evaluated sla)) err-sla-already-evaluated)
    (asserts! (is-eq (get status shipment) "created") err-invalid-status)
    
    (unwrap! (as-contract (stx-transfer? (+ (get penalty-amount sla) (get reward-amount sla)) tx-sender (get created-by sla))) err-payment-failed)
    
    (map-set sla-contracts
      { shipment-id: shipment-id }
      (merge sla {
        evaluated: true,
        evaluation-result: (some "cancelled"),
        status: "cancelled"
      })
    )
    
    (ok true)
  )
)

(define-read-only (get-sla-contract (shipment-id uint))
  (map-get? sla-contracts { shipment-id: shipment-id })
)

(define-read-only (check-sla-compliance (shipment-id uint))
  (let
    (
      (sla (unwrap! (map-get? sla-contracts { shipment-id: shipment-id }) (ok { compliant: false, reason: "sla-not-found" })))
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) (ok { compliant: false, reason: "shipment-not-found" })))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
      (quality-score (default-to u0 (get quality-score shipment)))
      (is-delivered (is-eq (get status shipment) "delivered"))
      (is-on-time (and is-delivered (<= (get last-updated shipment) (get delivery-deadline sla))))
      (meets-quality (>= quality-score (get quality-threshold sla)))
    )
    
    (ok {
      compliant: (and is-delivered is-on-time meets-quality),
      reason: (if (and is-delivered is-on-time meets-quality)
                "compliant"
                (if (not is-delivered)
                  "not-delivered"
                  (if (not is-on-time)
                    "late-delivery"
                    "quality-below-threshold")))
    })
  )
)

;; ===== DYNAMIC PRICING ENGINE =====
;; Automated pricing system based on market factors and route analytics

;; Data variables for pricing configuration
(define-data-var base-rate-per-km uint u5) ;; STX per kilometer
(define-data-var demand-multiplier-max uint u200) ;; max 200% of base rate
(define-data-var distance-discount-threshold uint u500) ;; km threshold for bulk discount
(define-data-var seasonal-adjustment uint u100) ;; percentage adjustment

;; Route demand and capacity tracking
(define-map route-analytics
  { origin-id: uint, destination-id: uint }
  {
    total-shipments: uint,
    active-shipments: uint,
    avg-delivery-time: uint,
    capacity-utilization: uint, ;; percentage 0-100
    demand-score: uint, ;; 0-100 relative demand
    last-updated: uint
  }
)

;; Cargo type pricing modifiers
(define-map cargo-type-modifiers
  { cargo-type: (string-ascii 50) }
  {
    base-modifier: uint, ;; percentage multiplier
    temperature-controlled: bool,
    hazardous: bool,
    fragile: bool,
    insurance-required: bool
  }
)

;; Historical pricing data for market analysis
(define-map pricing-history
  { route-key: (string-ascii 100), time-period: uint }
  {
    avg-price: uint,
    min-price: uint,
    max-price: uint,
    shipment-count: uint,
    success-rate: uint
  }
)

;; Dynamic pricing configuration per route
(define-map route-pricing-config
  { origin-id: uint, destination-id: uint }
  {
    distance-km: uint,
    base-price: uint,
    surge-threshold: uint, ;; active shipments that trigger surge pricing
    max-surge-multiplier: uint,
    off-peak-discount: uint,
    created-by: principal,
    active: bool
  }
)

;; Initialize default cargo type modifiers
(define-private (initialize-cargo-types)
  (begin
    (map-set cargo-type-modifiers
      { cargo-type: "standard" }
      { base-modifier: u100, temperature-controlled: false, hazardous: false, fragile: false, insurance-required: false })
    (map-set cargo-type-modifiers
      { cargo-type: "perishable" }
      { base-modifier: u130, temperature-controlled: true, hazardous: false, fragile: true, insurance-required: true })
    (map-set cargo-type-modifiers
      { cargo-type: "hazardous" }
      { base-modifier: u180, temperature-controlled: false, hazardous: true, fragile: false, insurance-required: true })
    (map-set cargo-type-modifiers
      { cargo-type: "fragile" }
      { base-modifier: u115, temperature-controlled: false, hazardous: false, fragile: true, insurance-required: true })
    (map-set cargo-type-modifiers
      { cargo-type: "bulk" }
      { base-modifier: u85, temperature-controlled: false, hazardous: false, fragile: false, insurance-required: false })
  )
)

;; Set up route pricing configuration
(define-public (configure-route-pricing 
  (origin-id uint) 
  (destination-id uint) 
  (distance-km uint) 
  (surge-threshold uint))
  (let
    (
      (company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
      (company-id (get company-id company-data))
      (base-price (* distance-km (var-get base-rate-per-km)))
    )
    
    ;; Verify company subscription
    (asserts! (is-subscription-active company-id) err-not-subscriber)
    (asserts! (> distance-km u0) err-invalid-pricing-params)
    (asserts! (> surge-threshold u0) err-invalid-pricing-params)
    
    ;; Check if route already configured
    (asserts! (is-none (map-get? route-pricing-config { origin-id: origin-id, destination-id: destination-id })) 
              err-pricing-config-exists)
    
    (map-set route-pricing-config
      { origin-id: origin-id, destination-id: destination-id }
      {
        distance-km: distance-km,
        base-price: base-price,
        surge-threshold: surge-threshold,
        max-surge-multiplier: u250, ;; 250% max surge
        off-peak-discount: u90, ;; 10% discount during off-peak
        created-by: tx-sender,
        active: true
      }
    )
    
    ;; Initialize route analytics
    (map-set route-analytics
      { origin-id: origin-id, destination-id: destination-id }
      {
        total-shipments: u0,
        active-shipments: u0,
        avg-delivery-time: u0,
        capacity-utilization: u0,
        demand-score: u50, ;; start with medium demand
        last-updated: (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1)))
      }
    )
    
    (ok true)
  )
)

;; Calculate dynamic price for a shipment
(define-public (calculate-shipment-price 
  (origin-id uint) 
  (destination-id uint) 
  (cargo-type (string-ascii 50)) 
  (quantity uint))
  (let
    (
      (route-config (unwrap! (map-get? route-pricing-config { origin-id: origin-id, destination-id: destination-id })
                            err-route-not-found))
      (route-stats (default-to 
        { total-shipments: u0, active-shipments: u0, avg-delivery-time: u0, capacity-utilization: u0, demand-score: u50, last-updated: u0 }
        (map-get? route-analytics { origin-id: origin-id, destination-id: destination-id })))
      (cargo-modifier (default-to 
        { base-modifier: u100, temperature-controlled: false, hazardous: false, fragile: false, insurance-required: false }
        (map-get? cargo-type-modifiers { cargo-type: cargo-type })))
      (base-price (get base-price route-config))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    
    (asserts! (get active route-config) err-route-not-found)
    (asserts! (> quantity u0) err-invalid-pricing-params)
    
    (let
      (
        ;; Calculate demand-based pricing
        (demand-multiplier (calculate-demand-multiplier route-stats))
        ;; Apply cargo type modifier
        (cargo-adjusted-price (/ (* base-price (get base-modifier cargo-modifier)) u100))
        ;; Apply quantity discount for bulk shipments
        (quantity-discount (if (>= quantity u10) u95 u100)) ;; 5% discount for 10+ units
        ;; Calculate final price with all modifiers
        (demand-adjusted-price (/ (* cargo-adjusted-price demand-multiplier) u100))
        (final-price (/ (* demand-adjusted-price quantity-discount quantity) u100))
      )
      
      (ok {
        base-price: base-price,
        cargo-modifier: (get base-modifier cargo-modifier),
        demand-multiplier: demand-multiplier,
        quantity-discount: quantity-discount,
        final-price: final-price,
        estimated-delivery-time: (get avg-delivery-time route-stats)
      })
    )
  )
)

;; Calculate demand multiplier based on route analytics
(define-private (calculate-demand-multiplier (route-stats (tuple
  (total-shipments uint)
  (active-shipments uint)
  (avg-delivery-time uint)
  (capacity-utilization uint)
  (demand-score uint)
  (last-updated uint))))
  (let
    (
      (active-shipments (get active-shipments route-stats))
      (demand-score (get demand-score route-stats))
      (capacity-util (get capacity-utilization route-stats))
    )
    
    ;; Base multiplier starts at 100%
    (let
      (
        ;; Higher demand increases price (50-150% based on demand score)
        (demand-factor (+ u50 (/ demand-score u2)))
        ;; High capacity utilization increases price
        (capacity-factor (if (> capacity-util u80) u120 u100))
        ;; Combine factors but cap at maximum multiplier
        (raw-multiplier (/ (* demand-factor capacity-factor) u100))
        (combined-multiplier (if (> raw-multiplier (var-get demand-multiplier-max))
                               (var-get demand-multiplier-max)
                               raw-multiplier))
      )
      combined-multiplier
    )
  )
)

;; Update route analytics when shipment is created/updated
(define-public (update-route-analytics 
  (origin-id uint) 
  (destination-id uint) 
  (shipment-status (string-ascii 20)))
  (let
    (
      (current-stats (default-to 
        { total-shipments: u0, active-shipments: u0, avg-delivery-time: u0, capacity-utilization: u0, demand-score: u50, last-updated: u0 }
        (map-get? route-analytics { origin-id: origin-id, destination-id: destination-id })))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    
    (let
      (
        (new-active (if (is-eq shipment-status "created")
                      (+ (get active-shipments current-stats) u1)
                      (if (or (is-eq shipment-status "delivered") (is-eq shipment-status "rejected"))
                        (if (> (get active-shipments current-stats) u0)
                          (- (get active-shipments current-stats) u1)
                          u0)
                        (get active-shipments current-stats))))
        (new-total (if (is-eq shipment-status "created")
                     (+ (get total-shipments current-stats) u1)
                     (get total-shipments current-stats)))
        ;; Update demand score based on activity
        (new-demand-score (calculate-demand-score new-active new-total))
      )
      
      (map-set route-analytics
        { origin-id: origin-id, destination-id: destination-id }
        (merge current-stats {
          total-shipments: new-total,
          active-shipments: new-active,
          demand-score: new-demand-score,
          last-updated: current-time
        })
      )
      
      (ok true)
    )
  )
)

;; Calculate demand score based on shipment activity
(define-private (calculate-demand-score (active-shipments uint) (total-shipments uint))
  (if (is-eq total-shipments u0)
    u50 ;; Default medium demand for new routes
    (let
      (
        ;; Higher ratio of active to total indicates higher demand
        (raw-ratio (if (> total-shipments u0) 
                      (/ (* active-shipments u100) total-shipments)
                      u0))
        (activity-ratio (if (> raw-ratio u100) u100 raw-ratio))
        ;; Adjust demand score based on activity
        (demand-adjustment (/ activity-ratio u2))
      )
      (let
        (
          (raw-demand-score (+ u30 demand-adjustment))
        )
        (if (> raw-demand-score u100) u100 raw-demand-score) ;; Keep demand score between 30-100
      )
    )
  )
)

;; Get pricing estimate for potential shipment
(define-read-only (get-pricing-estimate 
  (origin-id uint) 
  (destination-id uint) 
  (cargo-type (string-ascii 50)) 
  (quantity uint))
  (match (calculate-shipment-price origin-id destination-id cargo-type quantity)
    success (ok success)
    error (err error)
  )
)

;; Get route analytics data
(define-read-only (get-route-analytics (origin-id uint) (destination-id uint))
  (map-get? route-analytics { origin-id: origin-id, destination-id: destination-id })
)

;; Get cargo type pricing modifier
(define-read-only (get-cargo-type-modifier (cargo-type (string-ascii 50)))
  (map-get? cargo-type-modifiers { cargo-type: cargo-type })
)

;; Get route pricing configuration
(define-read-only (get-route-pricing-config (origin-id uint) (destination-id uint))
  (map-get? route-pricing-config { origin-id: origin-id, destination-id: destination-id })
)

;; Admin function to update base pricing parameters
(define-public (update-pricing-parameters 
  (new-base-rate uint) 
  (new-demand-multiplier-max uint) 
  (new-distance-threshold uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (> new-base-rate u0) err-invalid-pricing-params)
    (asserts! (>= new-demand-multiplier-max u100) err-invalid-pricing-params)
    
    (var-set base-rate-per-km new-base-rate)
    (var-set demand-multiplier-max new-demand-multiplier-max)
    (var-set distance-discount-threshold new-distance-threshold)
    
    (ok true)
  )
)

;; Initialize the pricing engine with default cargo types
(begin
  (initialize-cargo-types)
)


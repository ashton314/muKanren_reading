#lang racket/base

(require "kanren.rkt")

(define-syntax simple-relation
  (syntax-rules ()
    [(_ (bindings ...) [(vals ...) ...])
     (conde [(== bindings vals) ...] ...)]
    [(_ (bindings ...) [(vals ...) ...] [litteral-conde-args ...] ...)
     (conde [(== bindings vals) ...] ... [litteral-conde-args ...] ...)]))

(define (access-role person company access-level)
  (simple-relation (person company access-level)
                   [('ashton  'spiff 'developer)
                    ('michael 'spiff 'admin)
                    ('jeron   'spiff 'admin)
                    ('ashton  'uofu  'guest)
                    ('kimball 'uofu  'grad)
                    ('eric    'uofu  'admin)
                    ('john    'uofu  'admin)
                    ('matthew 'uofu  'admin)
                    ('ashton  'byu   'grad)
                    ('kimball 'byu   'admin)]
                   [(== access-level 'guest)]))

(define (company-member company-name employee-name)
  (fresh (role) (access-role employee-name company-name role)))

(define (resource-access company role resource)
  (fresh (masquerade-role)
         (simple-relation (company role resource)
                          [('spiff 'developer 'directory)
                           ('spiff 'guest 'website)
                           ('spiff 'admin 'payroll)
                           ('uofu  'admin 'payroll)
                           ('byu   'admin 'payroll)
                           ('uofu  'grad  'directory)
                           ('byu   'grad  'directory)
                           ('uofu  'guest 'public-campus)
                           ('byu   'guest 'public-campus)
                           ('byu   'guest 'directory)]
                          [(== role 'admin) (resource-access company masquerade-role resource)]
                          [(disj (== company 'uofu) (== company 'byu))
                           (== role 'grad)
                           (resource-access company 'guest resource)])))

;; What rolls can access the u's public campus?
;; (run 3 (role) (resource-access 'uofu role 'public-campus))

;; Who can access the u's payroll?
;; (run 3 (person role) (resource-access 'uofu role 'payroll) (access-role person 'uofu role))

;; Who can access the directory?
;; (run 4 (person role) (resource-access 'uofu role 'directory) (access-role person 'uofu role))

;; Across all companies, what resources can Ashton see, and why?
;; (run* (resource/role resource role company) (access-role 'ashton company role) (resource-access company role resource) (== resource/role (list company role resource)))

;; The same for Kimball:
;; (run 10 (resource/role resource role company) (access-role 'kimball company role) (resource-access company role resource) (== resource/role (list company role resource)))

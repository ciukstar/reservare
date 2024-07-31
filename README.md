[En français](https://github.com/ciukstar/reservare/blob/master/README.fr.md)

[În română](https://github.com/ciukstar/reservare/blob/master/README.ro.md)

[На русском](https://github.com/ciukstar/reservare/blob/master/README.ru.md)


# Reservare

Online bookings for services and appointments

## Overview

[“Reservare”](https://reservare-i4rimw5qwq-de.a.run.app) allows businesses to register services and resources that a customer can book and/or schedule an appointment for.  

*Use Case Diagram*  
![Use Case Diagram](static/img/Reservare-UCD.svg)

## Book a service
*State Machine Diagram*  
![State Machine Diagram](static/img/Reservare-Book-Service-SMD.svg)

## Make an appointment
*State Machine Diagram*  
![State Machine Diagram](static/img/Reservare-Make-Appointment-SMD.svg)

## Superuser

* Username  
  ```$YESOD_SUPERUSER_USERNAME```
  
* Password  
  ```$YESOD_SUPERUSER_PASSWORD```
  
A superuser account is defined at deployment time. The superuser manages other users and grants or revokes administrator privileges to specific users.

## Integration with external APIs

* Email: [Gmail API](https://developers.google.com/gmail/api/guides)

  * Client id  
    ```$YESOD_GOOGLE_CLIENT_ID```
  * Client secret  
    ```$YESOD_GOOGLE_CLIENT_SECRET```

* Payment gateway: [Stripe](https://stripe.com/)
  * Public key  
    ```$YESOD_STRIPE_PK```
  * Secret key  
    ```$YESOD_STRIPE_SK```
    
  To simulate a successful payment, use test cards from the [following list](https://stripe.com/docs/testing?testing-method=card-numbers#cards).

  To simulate payments that the issuer declines, use test cards from the [following list](https://stripe.com/docs/testing?testing-method=card-numbers#declined-payments).

## Search Engine Optimization
* Google SEO  
  ```$YESOD_GOOGLE_SITE_VERIFICATION```
* Bing SEO  
  ```$YESOD_MS_VALIDATE```
* Yandex SEO  
  ```$YESOD_YANDEX_VERIFICATION```

## Basic entities

### User

A new user can [sign up](https://reservare-i4rimw5qwq-de.a.run.app/auth/login) using an existing Google account or using a verified email address. [Gmail API](https://developers.google.com/gmail/api/guides) is used as an intermediary to send verification links to the user's inbox.

A user can be grated the administrator role by a superuser or by another administrator. Only users with the administrator role have access to administrative data.

### Business
...

### Workspace
...

### Payment option
...

### Service
...

### Staff
...

### Assignement
...

### Schedule
...

### Booking
...


*Entity Relationship Diagram*  
![Entity Relationship Diagram](static/img/Reservare-ERD.svg)

## Demo

[Click here to see demo](https://reservare-i4rimw5qwq-de.a.run.app)

_* Click on the [![Demo user accounts](demo/button-demo-accounts.png)](https://reservare-i4rimw5qwq-de.a.run.app/auth/login) button to get a list of demo accounts_

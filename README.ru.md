[In english](https://github.com/ciukstar/reservare/blob/master/README.md)

[En français](https://github.com/ciukstar/reservare/blob/master/README.fr.md)

[În română](https://github.com/ciukstar/reservare/blob/master/README.ro.md)

# Reservare

Онлайн-бронирование услуг и встреч

## Обзор

[«Reservare»](https://reservareru-i4rimw5qwq-de.a.run.app) позволяет компаниям регистрировать услуги и ресурсы, которые клиент может зарезервировать и/или назначить встречу.  

*Диаграмма вариантов использования*  
![Use Case Diagram](static/img/Reservare-UCD.svg)

## Забронировать услугу
*Диаграмма конечного автомата*  
![State Machine Diagram](static/img/Reservare-Book-Service-SMD.svg)

## Записаться на прием
*Диаграмма конечного автомата*  
![State Machine Diagram](static/img/Reservare-Make-Appointment-SMD.svg)

## Суперпользователь
* Имя пользователя  
  ```$YESOD_SUPERUSER_USERNAME```
* Пароль  
  ```$YESOD_SUPERUSER_PASSWORD```
  
Учетная запись суперпользователя определяется во время развертывания. Суперпользователь управляет другими пользователями и предоставляет или отзывает права администратора конкретным пользователям.

## Интеграция с внешними API

* Электронная почта: [Gmail API](https://developers.google.com/gmail/api/guides)

  * Идентификатор клиента  
    ```$YESOD_GOOGLE_CLIENT_ID```
  * Секрет клиента  
    ```$YESOD_GOOGLE_CLIENT_SECRET```

* Платежный шлюз: [Stripe](https://stripe.com/)
  * Открытый ключ  
    ```$YESOD_STRIPE_PK```
  * Секретный ключ  
    ```$YESOD_STRIPE_SK```
    
  Для имитации успешного платежа используйте тестовые карты из [следующего списка](https://stripe.com/docs/testing?testing-method=card-numbers#cards).

  Для имитации платежей, которые отклоняет эмитент, используйте тестовые карты из [следующего списка](https://stripe.com/docs/testing?testing-method=card-numbers#declined-payments).

* Платежный шлюз: [ЮKassa](https://yookassa.ru/)
  * Идентификатор магазина  
    ```$YESOD_YOOKASSA_SHOP_ID```
  * Секретный ключ  
    ```$YESOD_YOOKASSA_SECRET_KEY```

  Для имитации успешного платежа используйте тестовые карты из [следующего списка](https://yookassa.ru/developers/payment-acceptance/testing-and-going-live/testing#test-bank-card-success).

  Для имитации платежей, от которых эмитент отклоняет, используйте тестовые карты из [следующего списка](https://yookassa.ru/developers/payment-acceptance/testing-and-going-live/testing#test-bank-card-cancellation-details).

## Поисковая оптимизация
* Google SEO  
  ```$YESOD_GOOGLE_SITE_VERIFICATION```
* Bing SEO  
  ```$YESOD_MS_VALIDATE```
* Yandex SEO  
  ```$YESOD_YANDEX_VERIFICATION```

## Базовые сущности

### Пользователь
Новый пользователь может [зарегистрироваться](https://reservareru-i4rimw5qwq-de.a.run.app/auth/login), используя существующую учетную запись Google или подтвержденный адрес электронной почты. [API Gmail](https://developers.google.com/gmail/api/guides) используется в качестве посредника для отправки ссылок проверки в почтовый ящик пользователя.

Пользователю может быть предоставлена роль администратора суперпользователем или другим администратором. Только пользователи с ролью администратора имеют доступ к административным данным.

### Деятельность
...

### Рабочее место
...

### Вариант оплаты
...

### Услуга
...

### Персонал
...

### Назначение
...

### График работы
...

### Бронирование
...


*Диаграмма отношений сущностей*  
![Entity Relationship Diagram](static/img/Reservare-ERD.svg)

## Демо

[Нажмите здесь, чтобы увидеть демо](https://reservareru-i4rimw5qwq-de.a.run.app)

_* Нажмите на кнопку [![Demo user accounts](demo/button-demo-accounts.png)](https://reservareru-i4rimw5qwq-de.a.run.app/auth/login), чтобы получить список демонстрационных учетных записей пользователей_


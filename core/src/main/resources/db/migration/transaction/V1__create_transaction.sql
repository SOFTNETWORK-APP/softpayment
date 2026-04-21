CREATE TABLE IF NOT EXISTS transaction (
  id                                VARCHAR(255)  NOT NULL PRIMARY KEY,
  order_uuid                        VARCHAR(255)  NOT NULL,
  nature                            VARCHAR(20)   NOT NULL,
  transaction_type                  VARCHAR(30)   NOT NULL,
  status                            VARCHAR(50)   NOT NULL,
  amount                            INTEGER       NOT NULL,
  currency                          VARCHAR(3)    NOT NULL DEFAULT 'EUR',
  fees                              INTEGER       NOT NULL DEFAULT 0,
  result_code                       VARCHAR(50)   NOT NULL,
  result_message                    TEXT          NOT NULL,
  author_id                         VARCHAR(255)  NOT NULL,
  payment_type                      VARCHAR(20)   NOT NULL DEFAULT 'CARD',
  created_date                      TIMESTAMP     NOT NULL,
  last_updated                      TIMESTAMP     NOT NULL,
  payment_method_id                 VARCHAR(255),
  redirect_url                      TEXT,
  reason_message                    TEXT,
  credited_wallet_id                VARCHAR(255),
  credited_user_id                  VARCHAR(255),
  debited_wallet_id                 VARCHAR(255),
  debited_user_id                   VARCHAR(255),
  mandate_id                        VARCHAR(255),
  pre_authorization_id              VARCHAR(255),
  recurring_pay_in_registration_id  VARCHAR(255),
  external_reference                VARCHAR(255),
  pre_authorization_validated       BOOLEAN,
  pre_authorization_canceled        BOOLEAN,
  pre_authorization_expired         BOOLEAN,
  pre_authorization_debited_amount  INTEGER,
  return_url                        TEXT,
  paypal_payer_email                VARCHAR(255),
  idempotency_key                   VARCHAR(255),
  client_id                         VARCHAR(255),
  payment_client_secret             TEXT,
  payment_client_data               TEXT,
  payment_client_return_url         TEXT,
  source_transaction_id             VARCHAR(255),
  transfer_amount                   INTEGER,
  pre_registration_id               VARCHAR(255),
  paypal_payer_id                   VARCHAR(255),
  pay_in_id                         VARCHAR(255),
  deleted                           BOOLEAN       NOT NULL DEFAULT FALSE
);

CREATE INDEX idx_transaction_order_uuid ON transaction(order_uuid);
CREATE INDEX idx_transaction_author_id ON transaction(author_id);
CREATE INDEX idx_transaction_status ON transaction(status);
CREATE INDEX idx_transaction_type ON transaction(transaction_type);
CREATE INDEX idx_transaction_client_id ON transaction(client_id);
CREATE INDEX idx_transaction_recurring ON transaction(recurring_pay_in_registration_id);
CREATE INDEX idx_transaction_payment_type ON transaction(payment_type);

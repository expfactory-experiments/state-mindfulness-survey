library(tidyverse)
library(efsmsr)

df <- expfactoryr::process_expfactory_survey(token='1', survey='tests/fixtures/sms.json') %>%
  mutate(p = 1)
flat <- expfactoryr::process_expfactory_survey(token='1', survey='tests/fixtures/sms-flat.json', flat=TRUE) %>%
  rename(p = Token)
sms <- state_mindfulness_scale(flat)
test_that('SMS total value', {
  expect_equal(sms[1,'sms_total'], 92)
})

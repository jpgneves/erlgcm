-ifndef(_ERLGCM_HRL).
-define(_ERLGCM_HRL, true).

-ifdef(DEBUG).
-define(GCM_SEND_ENDPOINT, "http://android.googleapis.com/gcm/send").
-else.
-define(GCM_SEND_ENDPOINT, "https://android.googleapis.com/gcm/send").
-endif.

-define(PARAM_REGISTRATION_IDS, registration_ids).

-define(PARAM_COLLAPSE_KEY, collapse_key).

-define(PARAM_DELAY_WHILE_IDLE, delay_while_idle).

-define(PARAM_PAYLOAD, data).

-define(PARAM_PAYLOAD_PREFIX, "data.").

-define(PARAM_TIME_TO_LIVE, time_to_live).

-define(ERROR_QUOTA_EXCEEDED, quota_exceeded).

-define(ERROR_DEVICE_QUOTA_EXCEEDED, device_quota_exceeded).

-define(ERROR_MISSING_REGISTRATION, missing_registration).

-define(ERROR_INVALID_REGISTRATION, invalid_registration).

-define(ERROR_MISMATCH_SENDER_ID, mismatch_sender_id).

-define(ERROR_NOT_REGISTERED, not_registered).
-define(ERROR_MESSAGE_TOO_BIG, message_too_big).

-define(ERROR_MISSING_COLLAPSE_KEY, missing_collapse_key).

-define(ERROR_UNAVAILABLE, unavailable).

-define(TOKEN_MESSAGE_ID, "id").

-define(TOKEN_CANONICAL_REG_ID, "registration_id").

-define(TOKEN_ERROR, "Error").

-define(JSON_REGISTRATION_IDS, registration_ids).

-define(JSON_PAYLOAD, data).

-define(JSON_SUCCESS, success).

-define(JSON_FAILURE, failure).

-define(JSON_CANONCIAL_IDS, canonical_ids).

-define(JSON_MULTICAST_ID, multicast_id).

-define(JSON_RESULTS, results).

-define(JSON_ERROR, error).

-define(JSON_MESSAGE_ID, message_id).

-define(CONTENT_TYPE_JSON, "application/json").

-endif.

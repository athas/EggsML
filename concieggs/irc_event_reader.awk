# Requires GNU awk.

BEGIN {
  name = ENVIRON["CONCIEGGS_NAME"]
  error_channel = ENVIRON["CONCIEGGS_ERROR_CHANNEL"]
  default_channel = ENVIRON["CONCIEGGS_DEFAULT_CHANNEL"]
}

function shellquote(str) {
  gsub(/'/, "'\\''", str)
  return sprintf("'%s'", str)
}

# Valid IRC message at all?
{
  context=0
  timestamp=0
  payload=0
  message_from=0
  message_body=0
  server_message_code=0
  if (match($0, /^([^ ']+) *: ([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]) (.*)$/, matches)) {
    context=matches[1]
    timestamp=matches[2]
    payload=matches[3]
  } else {
    # Invalid.
    next
  }
}

# Server message?
match(payload, /^>< ([0-9][0-9][0-9]) \(([^)]*)\): (.*)$/, matches) {
  server_message_code=matches[1]
  server_message_arg=matches[2]
  server_message_body=matches[3]
  # FIXME: Actually make some hooks for this.
  if (server_message_code == "001") {
    system("export EGGS_USER=" shellquote(context) "\n"                 \
           "export EGGS_WHERE=" shellquote(shell_message_body) "\n"     \
           "runFor \"$EGGS_WHERE\" runHooks server_connect")
  }
}

# Part action?
match(payload, /^>< (PART|JOIN) \(([^)]+)\): (.*)$/, matches) {
  action=matches[1]
  partedUser=context # yes
  if (partedUser == name) {
    next
  }
  parted_channel=matches[2]
  partedReason=matches[3]
  if (action == "PART") {
    runHooks="runHooks channel_part"
  } else {
    runHooks="runHooks channel_join"
  }
  system("export EGGS_USER=" shellquote(context) "\n"   \
         "export EGGS_WHERE=" shellquote(parted_channel) "\n"          \
         "export EGGS_BODY=" shellquote(partedReason) "\n"              \
         "runFor \"$EGGS_WHERE\" " runHooks )
}

# Channel/private message?
match(payload, /^<([^ ]+)> (.*)$/, matches) {
  message_from=matches[1]
  message_body=matches[2]
  if (message_from == name) {
    next
  }
  setvars=("export EGGS_USER=" shellquote(message_from) "\n"             \
           "export EGGS_WHERE=" shellquote(context) "\n"          \
           "export EGGS_WHEN=" shellquote(timestamp) "\n"               \
           "export EGGS_BODY=" shellquote(message_body) "\n" \
           "export EGGS_LINE=" shellquote($0) "\n")
  if (match(context, /^#/)) {
    system(setvars "\n" "runFor \"$EGGS_WHERE\" runHooks channel_message")
  } else {
    system(setvars "\n" "runFor \"$EGGS_USER\" runHooks private_message")
  }
}

# CLatter Command Testing Guide

This document provides test commands for all CLatter IRC commands. Commands are organized by category with safe test examples.

## Basic Communication

### /join
Join a channel.
```
/join #clatter
/join #test-channel secretkey
```

### /part (alias: /leave)
Leave a channel.
```
/part
/part Goodbye!
/part #clatter See you later
```

### /msg (alias: /privmsg)
Send a private message.
```
/msg NickServ help
/msg glenneth Hello there!
```

### /me
Send an action message.
```
/me waves hello
/me is testing CLatter
```

### /query
Open a private message buffer with a user.
```
/query glenneth
```

### /nick
Change your nickname.
```
/nick glenneth_test2
```

## Server Commands

### /quit (alias: /q)
Disconnect from IRC.
```
/quit
/quit Goodbye everyone!
```

### /raw
Send a raw IRC protocol command.
```
/raw PING :test
/raw PRIVMSG #clatter :Hello from raw!
/raw VERSION
```

### /whois
Query user information.
```
/whois glenneth
/whois NickServ
```

### /who
Query user information (different format).
```
/who glenneth
/who #clatter
```

### /list
List channels on server.
```
/list
/list *lisp*
/list #cl*
```

## Channel Management

### /topic
View or set channel topic.
```
/topic
/topic New topic for the channel
```

### /kick
Kick a user from channel (requires op).
```
/kick baduser
/kick baduser Spamming
```

### /ban
Ban a user from channel (requires op).
```
/ban baduser
```

### /unban
Remove a ban (requires op).
```
/unban baduser
```

### /mode
View or set channel/user modes.
```
/mode
/mode +o glenneth
/mode #clatter +nt
```

### /invite
Invite a user to a channel.
```
/invite glenneth
/invite glenneth #clatter
```

### /names
Refresh the member list for a channel.
```
/names
/names #clatter
```

### /members
Show the current member list (local cache).
```
/members
```

## Services Shortcuts

### /ns
Send a command to NickServ.
```
/ns help
/ns info glenneth
/ns status
```

### /cs
Send a command to ChanServ.
```
/cs help
/cs info #clatter
```

## CTCP Commands

### /ctcp
Send a CTCP request to a user.
```
/ctcp glenneth VERSION
/ctcp glenneth PING
/ctcp glenneth TIME
```

## Buffer Management

### /close
Close the current buffer (use after /part).
```
/close
```

## Configuration

### /autojoin
Manage autojoin channels.
```
/autojoin
/autojoin list
/autojoin add #newchannel
/autojoin remove #oldchannel
```

### /ignore
Manage ignored users.
```
/ignore
/ignore spammer
```

### /unignore
Remove a user from ignore list.
```
/unignore spammer
```

## Away Status

### /away
Set yourself as away.
```
/away
/away Gone for lunch
```

### /back
Clear away status.
```
/back
```

## Logging

### /log
View or search logs.
```
/log
/log list
/log search keyword
/log export text
/log export json ~/logs/export.json
/log export html
```

## Chat History (IRCv3)

### /history
Request chat history from server (if supported).
```
/history
/history 100
```

## Monitor (IRCv3)

### /monitor
Track users online/offline status.
```
/monitor
/monitor l
/monitor + nick1,nick2
/monitor - nick1
/monitor c
/monitor s
```

## DCC Commands

### /dcc
Direct Client-to-Client connections.
```
/dcc list
/dcc chat glenneth
/dcc send glenneth /path/to/file.txt
/dcc accept
/dcc accept 1
/dcc reject
/dcc reject 1
```

**Note:** DCC requires direct network connectivity. May not work through NAT without port forwarding.

## File Sharing

### /crafterbin
Upload a file to CrafterBin and copy URL to clipboard.
```
/crafterbin /path/to/file.txt
/crafterbin ~/screenshot.png
```

## Help

### /help
Show help for all commands.
```
/help
```

---

## Testing Checklist

### Tested and Passed (2026-01-10):
- [x] /help - Auto-generated from CLOS registry
- [x] /join #channel - Joins and adds to autojoin
- [x] /part - Leaves channel
- [x] /msg NickServ help - Sends PM
- [x] /me action - Sends action
- [x] /nick newnick - Changes nick
- [x] /ns info - NickServ shortcut
- [x] /cs info - ChanServ shortcut
- [x] /query nick - Opens query buffer (switches to it)
- [x] /whois nick - Queries user info
- [x] /topic - Views topic
- [x] /away message - Sets away
- [x] /back - Clears away
- [x] /quit - Disconnects cleanly
- [x] /ctcp VERSION - Returns client version
- [x] /ctcp PING - Returns ping response
- [x] /ctcp TIME - Returns time
- [x] /close - Closes buffer
- [x] /who - Query user info
- [x] /names - Refresh member list
- [x] /ignore - List/toggle ignored users
- [x] /unignore - Remove from ignore
- [x] /members - Show channel members
- [x] /list - List channels on server
- [x] /autojoin - List/manage autojoin channels
- [x] /log - View/search local logs
- [x] /mode - View channel modes
- [x] /raw - Send raw IRC commands

### Requires own channel or op status:
- [ ] /kick
- [ ] /ban
- [ ] /unban
- [ ] /mode +o/-o
- [ ] /topic (set)
- [ ] /invite

### Requires coordination with another user:
- [ ] /dcc chat
- [ ] /dcc send
- [ ] /msg (to real user)

### Modifies config (tested):
- [x] /autojoin add/remove
- [x] /ignore nick
- [x] /unignore nick

### Server-dependent (IRCv3):
- [ ] /history
- [ ] /monitor

### Disconnects:
- [ ] /quit (tested, works)

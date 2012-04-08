{-# LANGUAGE TemplateHaskell #-}
module Clckwrks.IrcBot.URL where

import Web.Routes.TH         (derivePathInfo)

data IrcBotAdminURL
    = IrcBotReconnect
    | IrcBotSettings
$(derivePathInfo ''IrcBotAdminURL)

data IrcBotURL
    = IrcLogs
    | IrcLog FilePath
    | IrcBotAdmin IrcBotAdminURL
$(derivePathInfo ''IrcBotURL)

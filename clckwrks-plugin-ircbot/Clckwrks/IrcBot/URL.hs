{-# LANGUAGE TemplateHaskell #-}
module Clckwrks.IrcBot.URL where

import Web.Routes.TH         (derivePathInfo)

data IrcBotAdminURL
    = IrcBotAdminHome
$(derivePathInfo ''IrcBotAdminURL)

data IrcBotURL
    = IrcLogs
    | IrcLog FilePath
    | IrcBotAdmin IrcBotAdminURL
$(derivePathInfo ''IrcBotURL)

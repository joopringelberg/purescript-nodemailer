module NodeMailer
  ( NODEMAILER
  , AuthConfig
  , ExtendableTransportConfig
  , TransportConfig(..)
  , HostTransportConfig
  , ServiceTransportConfig
  , Message
  , Transporter
  , createTransporter
  , sendMail
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (Eff, kind Effect)
import Data.Function.Uncurried (Fn2, runFn2)



type AuthConfig =
  { user :: String
  , pass :: String
  }

type ExtendableTransportConfig f = { auth :: AuthConfig | f }

type HostTransportConfig = ExtendableTransportConfig
  ( host :: String
  , port :: Int
  , secure :: Boolean
  )

type ServiceTransportConfig = ExtendableTransportConfig
  ( service :: String )

data TransportConfig = HostConfig HostTransportConfig | ServiceConfig ServiceTransportConfig

type Message f =
  { from :: String
  , to :: Array String
  , subject :: String
  , text :: String
  | f
  }

foreign import data Transporter :: Type

foreign import data NODEMAILER :: Effect



sendMail :: forall e f. (Message f) -> Transporter -> Aff (nodemailer :: NODEMAILER | e) Unit
sendMail message transporter = fromEffFnAff $ runFn2 _sendMail message transporter

createTransporter :: forall e. TransportConfig -> Eff (nodemailer :: NODEMAILER | e) Transporter
createTransporter (HostConfig c) = _createTransporter c
createTransporter (ServiceConfig c) = _createTransporter c

foreign import _createTransporter :: forall e f. (ExtendableTransportConfig f) -> Eff (nodemailer :: NODEMAILER | e)  Transporter



foreign import _sendMail :: forall e f. Fn2 (Message f) Transporter (EffFnAff (nodemailer :: NODEMAILER | e) Unit)

module NodeMailer
  ( NODEMAILER
  , AuthConfig
  , ExtendableTransportConfig
  , TransportConfig(..)
  , HostTransportConfig
  , ServiceTransportConfig
  , Message
  , Transporter
  , MessageInfo
  , createTransporter
  , createTestAccount
  , getTestMessageUrl
  , sendMail
  , sendMail_
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign (Foreign)
import NodeMailer.Attachment (Attachment)
import Simple.JSON (write)

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

type TestAccount =
  { user :: String
  , pass :: String
  , smtp :: { host :: String, port :: Int, secure :: Boolean }
  }

type Message f =
  { from :: String
  , to :: Array String
  , cc :: Array String
  , bcc :: Array String
  , subject :: String
  , text :: String
  , attachments :: Array Attachment
  | f
  }

foreign import data Transporter :: Type

foreign import data MessageInfo :: Type

sendMail :: forall f. Message f -> Transporter -> Aff Unit
sendMail message transporter = void $ sendMail_ message transporter

createTransporter :: forall e. TransportConfig -> Eff (nodemailer :: NODEMAILER | e) Transporter
createTransporter (HostConfig c) = _createTransporter c
createTransporter (ServiceConfig c) = _createTransporter c

foreign import _createTransporter :: forall e f. (ExtendableTransportConfig f) -> Eff (nodemailer :: NODEMAILER | e)  Transporter

sendMail_ :: forall f. Message f -> Transporter -> Aff MessageInfo
sendMail_ message transporter = fromEffectFnAff $ runFn2 _sendMail (write message) transporter

createTestAccount :: Aff TransportConfig
createTestAccount = do
  account <- fromEffectFnAff _createTestAccount
  pure
    { host: account.smtp.host
    , port: account.smtp.port
    , secure: account.smtp.secure
    , auth: { user: account.user, pass: account.pass }
    }

getTestMessageUrl :: MessageInfo -> Maybe String
getTestMessageUrl = runFn3 _getTestMessageUrl Nothing Just

foreign import createTransporter :: TransportConfig -> Effect Transporter

foreign import _sendMail :: Fn2 Foreign Transporter (EffectFnAff MessageInfo)

foreign import _createTestAccount :: EffectFnAff TestAccount

foreign import _getTestMessageUrl
  :: Fn3 (Maybe String) (String -> Maybe String) MessageInfo (Maybe String)

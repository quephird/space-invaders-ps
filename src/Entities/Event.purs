module Entities.Event where

import Prelude ( Eq )

import Optic.Core ( lens )

data Status = Handled
            | New

instance eqStatus :: Eq Status where
  eq Handled Handled = true
  eq New     New     = true
  eq _       _       = false

data EventType = NewInvaderBullet
               | NewMysteryBullet
               | NewMysteryShip
               | GoneMysteryShip
               | InvaderShot
               | MysteryShipShot
               | PlayerShot

data Event = Event EventType Status

status = lens (\(Event _ status) -> status)
              (\(Event eventType status) status' -> Event eventType status')

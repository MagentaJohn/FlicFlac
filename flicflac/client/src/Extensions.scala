package game

import indigo.*
import indigoextras.ui.*
import scribe.*

// the original extensions to Button and Pointers was kindly provided by Dave Smith

/* This is a workaround to show a way to make buttons support simple pointer events.
 * It is a simplified version of the standard Button update
 * function.
 */
extension (b: Button)
  def updateFromPointers(p: Pointers): Outcome[Button] =
    val inBounds = b.bounds.isPointWithin(p.position)

    val hoverEvents: Batch[GlobalEvent] =
      if inBounds && p.moved then b.onHoverOver()
      else Batch.empty

    val upEvents: Batch[GlobalEvent] =
      if inBounds && p.released then b.onUp()
      else Batch.empty

    val downEvents: Batch[GlobalEvent] =
      if inBounds && p.pressed then b.onDown()
      else Batch.empty

    val pointerEvents: Batch[GlobalEvent] =
      hoverEvents ++ downEvents ++ upEvents

    b.state match

      // Stay in Down state
      case ButtonState.Down if inBounds && p.pressed =>
        Outcome(b).addGlobalEvents(b.onHoldDown() ++ pointerEvents)

      // Move from Up to Down state on button/pointer press
      case ButtonState.Up if inBounds && p.pressed =>
        Outcome(b.toDownState).addGlobalEvents(b.onHoverOver() ++ pointerEvents)

      // Move from Down to Up state (out of bounds)
      case ButtonState.Down if !inBounds && (p.pressed || p.released || p.moved) =>
        Outcome(b.toUpState).addGlobalEvents(b.onHoverOut() ++ pointerEvents)

      // Move from Down to Up state (mouse/pointer released)
      case ButtonState.Down if inBounds && p.released =>
        Outcome(b.toUpState).addGlobalEvents(pointerEvents)

      // Move from Over to Up state (out of bounds)
      case ButtonState.Over if !inBounds && (p.pressed || p.released || p.moved) =>
        Outcome(b.toUpState).addGlobalEvents(b.onHoverOut() ++ pointerEvents)

      // Move from Over to Down state (mouse/pointer press)
      case ButtonState.Over if inBounds && p.pressed =>
        Outcome(b.toDownState).addGlobalEvents(b.onDown() ++ pointerEvents)

      // Move from Up to Over (mouse/pointer moved within bounds)
      case ButtonState.Up if inBounds && p.moved =>
        Outcome(b.toOverState).addGlobalEvents(b.onHoverOver() ++ pointerEvents)

      // Unaccounted for states.
      case _ =>
        Outcome(b).addGlobalEvents(pointerEvents)
    end match
  ; // dummy statement here helps scalafmt to format correctly

end extension

/** This is a workaround to make up for Pointer not exposing any convenience methods.
  */
extension (p: Pointers)

  def pressed: Boolean =
    p.pointerEvents.exists {
      case _: PointerEvent.PointerDown => true
      case _                           => false
    }

  def released: Boolean =
    p.pointerEvents.exists {
      case _: PointerEvent.PointerUp => true
      case _                         => false
    }

  def moved: Boolean =
    p.pointerEvents.exists {
      case _: PointerEvent.PointerMove => true
      case _                           => false
    }
end extension

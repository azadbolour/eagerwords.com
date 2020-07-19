import {createSelector} from 'reselect';
import {mkGameHandler} from "../service/GameHandler";
import {selectLoginEvidence, selectIsGuest} from "../../auth/redux/AuthSelector"

function selectSlice(state, props) {
  return state.gameReducer;
}

export const selectGameIdToResume = createSelector(
  selectSlice,
  (slice) => slice.gameIdToResume
);

// TODO. URGENT. Reduce coupling. Move special constructor to GameHandler.
export const selectGameHandler = _ => createSelector(
  selectIsGuest,
  selectLoginEvidence,
  (isGuest, evidence) =>
    mkGameHandler(isGuest ? null : evidence)
);

package com.fy.navi.ui.dialog;

import com.fy.navi.ui.define.TripID;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/31
 */
public interface IBaseDialogClickListener {
    default void onShowListener() {

    }

    default void onCommitClick() {

    }

    default void onCommitClick(TripID tripID) {

    }

    default void onCancelClick() {

    }
}

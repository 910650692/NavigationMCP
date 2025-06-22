package com.sgm.navi.ui.dialog;

import com.sgm.navi.ui.define.TripID;

public interface IBaseDialogClickListener {
    /**
     * 显示监听
     */
    default void onShowListener() {

    }

    /**
     * 确定按钮点击监听
     */
    default void onCommitClick() {

    }

    /**
     * 确定按钮点击监听
     *
     * @param tripID TripID
     */
    default void onCommitClick(TripID tripID) {

    }

    /**
     * 取消按钮点击监听
     */
    default void onCancelClick() {

    }

    default void onNetWorkConnect(){

    }

    default void onExit(){

    }
}

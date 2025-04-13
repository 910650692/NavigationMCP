package com.fy.navi.service.logicpaket.user.behavior;

public interface FavoriteStatusCallback {

    /**
     * 家状态变更
     * @param isSet true: 已设置，false:未设置
     */
    default void notifyFavoriteHomeChanged(boolean isSet) {

    }

    /**
     * 公司状态变更
     * @param isSet isSet true: 已设置，false:未设置
     */
    default void notifyFavoriteCompanyChanged(boolean isSet){

    }
}

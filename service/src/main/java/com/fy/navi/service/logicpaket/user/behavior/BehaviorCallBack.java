package com.fy.navi.service.logicpaket.user.behavior;

import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;

public interface BehaviorCallBack {

    /**
     * notifyFavorite
     * @param eventType
     * @param exCode
     */
    void notifyFavorite(int eventType, int exCode);

    /**
     * notifyFavoriteAsync
     * @param type
     * @param data
     * @param sorted
     */
    void notifyFavoriteAsync(int type, ArrayList<PoiInfoEntity> data, boolean sorted);
}
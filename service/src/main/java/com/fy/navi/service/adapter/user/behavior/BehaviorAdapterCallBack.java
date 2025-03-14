package com.fy.navi.service.adapter.user.behavior;


import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;

/**
 * @Description
 * @Author fh
 * @date 2024/12/26
 */
public interface BehaviorAdapterCallBack {

    void notifyFavorite(int eventType, int exCode);

    void notifyFavoriteAsync(int type, ArrayList<PoiInfoEntity> data, boolean sorted);

}

package com.fy.navi.service.logicpaket.user.behavior;

import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;

/**
 * @Description 数据收藏 callback回调
 * @Author fh
 * @date 2024/12/26
 */
public interface BehaviorCallBack {

    void notifyFavorite(int eventType, int exCode);

    void notifyFavoriteAsync(int type, ArrayList<PoiInfoEntity> data, boolean sorted);
}
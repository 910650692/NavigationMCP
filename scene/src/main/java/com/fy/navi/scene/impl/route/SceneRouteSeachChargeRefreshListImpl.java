package com.fy.navi.scene.impl.route;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.route.ISceneRouteSearchRefreshList;
import com.fy.navi.scene.ui.route.SceneRouteSearchChargeRefreshListView;
import com.fy.navi.service.define.search.PoiInfoEntity;

public class SceneRouteSeachChargeRefreshListImpl extends BaseSceneModel<SceneRouteSearchChargeRefreshListView>
        implements ISceneRouteSearchRefreshList {
    public SceneRouteSeachChargeRefreshListImpl(final SceneRouteSearchChargeRefreshListView screenView) {
        super(screenView);
    }
    @Override
    public void onItermAddClick(final PoiInfoEntity poiInfoEntity) {

    }

    /**
     * 删除途经点
     *
     * @param poiInfoEntity poi信息
     */
    @Override
    public void onItermRemoveClick(final PoiInfoEntity poiInfoEntity) {

    }
}

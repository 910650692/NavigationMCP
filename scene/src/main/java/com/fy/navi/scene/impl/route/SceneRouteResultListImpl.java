package com.fy.navi.scene.impl.route;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.route.ISceneRouteResultList;
import com.fy.navi.scene.ui.route.SceneRouteResultListView;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class SceneRouteResultListImpl extends BaseSceneModel<SceneRouteResultListView> implements ISceneRouteResultList {

    private final RoutePackage mRoutePackage;

    public SceneRouteResultListImpl(SceneRouteResultListView mScreenView) {
        super(mScreenView);
        mRoutePackage = RoutePackage.getInstance();
    }

    @Override
    public void selectRoute(int index) {
        mRoutePackage.selectRoute(mMapTypeId, index);
    }
}

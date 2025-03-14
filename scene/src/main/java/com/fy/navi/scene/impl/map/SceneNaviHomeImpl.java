package com.fy.navi.scene.impl.map;

import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.map.ISceneCalculate;
import com.fy.navi.scene.ui.map.SceneNaviHomeView;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.logicpaket.route.RoutePackage;

/***
 * @author yaWei
 * @desc 导航回家逻辑实现
 */
public class SceneNaviHomeImpl extends BaseSceneModel<SceneNaviHomeView>  implements ISceneCalculate {
    private static final String TAG = "SceneNaviHomeImpl";
    private final RoutePackage mRoutePackage;

    public SceneNaviHomeImpl(SceneNaviHomeView mScreenView) {
        super(mScreenView);
        mRoutePackage = RoutePackage.getInstance();
    }

    @Override
    public void calculateRoad(@Nullable RouteParam routeParam) {
        if (routeParam != null) {
            // TODO 发起算路逻辑
        } else {
            Logger.e(TAG, "routeParam == null，无法正确算路！");
        }
    }
}

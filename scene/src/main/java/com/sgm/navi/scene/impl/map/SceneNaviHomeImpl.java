package com.sgm.navi.scene.impl.map;

import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.api.map.ISceneCalculate;
import com.sgm.navi.scene.ui.map.SceneNaviHomeView;
import com.sgm.navi.service.define.route.RouteParam;

/***
 * @author yaWei
 * @desc 导航回家逻辑实现
 */
public class SceneNaviHomeImpl extends BaseSceneModel<SceneNaviHomeView>  implements ISceneCalculate {
    private static final String TAG = "SceneNaviHomeImpl";
    public SceneNaviHomeImpl(SceneNaviHomeView screenView) {
        super(screenView);
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

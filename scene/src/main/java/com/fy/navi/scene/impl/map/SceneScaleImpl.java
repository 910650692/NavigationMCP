package com.fy.navi.scene.impl.map;

import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.map.ISceneScale;
import com.fy.navi.scene.ui.map.SceneScaleView;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.logicpaket.map.MapPackage;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class SceneScaleImpl extends BaseSceneModel<SceneScaleView> implements ISceneScale {
    private MapPackage mapPackage;

    public SceneScaleImpl(SceneScaleView mScreenView) {
        super(mScreenView);
        mapPackage = MapPackage.getInstance();
    }

    @Override
    public void reduceLevel() {
        Logger.i("lvww", "缩小比例尺");
        mapPackage.reduceLevel(MapTypeId.MAIN_SCREEN_MAIN_MAP);
    }

    @Override
    public void amplifyLevel() {
        Logger.i("lvww", "放大比例尺");
        mapPackage.amplifyLevel(MapTypeId.MAIN_SCREEN_MAIN_MAP);
    }
}

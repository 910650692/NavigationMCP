package com.sgm.navi.scene.impl.navi;


import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.ui.navi.SceneNaviServiceArea;
import com.sgm.navi.service.adapter.layer.LayerAdapter;
import com.sgm.navi.service.logicpaket.mapdata.MapDataPackage;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.position.PositionPackage;

public class SceneNaviDetailImpl extends BaseSceneModel<SceneNaviServiceArea>  {
    private final NaviPackage mNaviPackage;
    private final PositionPackage mPositionPackage;
    private final MapDataPackage mMapDataPackage;
    private LayerAdapter mLayerAdapter;

    public SceneNaviDetailImpl(final SceneNaviServiceArea screenView) {
        super(screenView);
        mPositionPackage = PositionPackage.getInstance();
        mMapDataPackage = MapDataPackage.getInstance();
        mNaviPackage = NaviPackage.getInstance();
        mLayerAdapter = LayerAdapter.getInstance();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
    }
}

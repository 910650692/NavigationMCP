package com.sgm.navi.scene.impl.poi;

import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.api.poi.IScenePoiChargePriceListView;
import com.sgm.navi.scene.ui.poi.ScenePoiChargePriceListView;

public class ScenePoiChargePriceListViewImpl extends BaseSceneModel<ScenePoiChargePriceListView> implements IScenePoiChargePriceListView {
    public ScenePoiChargePriceListViewImpl(ScenePoiChargePriceListView mScreenView) {
        super(mScreenView);
    }

    @Override
    public void closeFragment() {
        mScreenView.closeFragment();
    }
}

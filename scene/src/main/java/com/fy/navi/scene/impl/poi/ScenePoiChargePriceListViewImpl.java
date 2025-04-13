package com.fy.navi.scene.impl.poi;

import android.util.Log;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.poi.IScenePoiChargePriceListView;
import com.fy.navi.scene.ui.poi.ScenePoiChargePriceListView;
import com.fy.navi.ui.base.StackManager;

public class ScenePoiChargePriceListViewImpl extends BaseSceneModel<ScenePoiChargePriceListView> implements IScenePoiChargePriceListView {
    public ScenePoiChargePriceListViewImpl(ScenePoiChargePriceListView mScreenView) {
        super(mScreenView);
    }

    @Override
    public void closeFragment() {
        mScreenView.closeFragment();
    }
}

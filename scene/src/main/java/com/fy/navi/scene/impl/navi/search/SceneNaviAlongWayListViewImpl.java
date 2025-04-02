package com.fy.navi.scene.impl.navi.search;

import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.ui.navi.search.SceneNaviAlongWayListView;

public class SceneNaviAlongWayListViewImpl extends BaseSceneModel<SceneNaviAlongWayListView> {

    public static final String TAG = "SceneNaviAlongWayListViewImpl";
    public SceneNaviAlongWayListViewImpl(SceneNaviAlongWayListView mScreenView) {
        super(mScreenView);
    }

    public void closeSearchView() {
        Logger.i(TAG, "closeSearchView");
        mScreenView.closeSearchView();
    }
}

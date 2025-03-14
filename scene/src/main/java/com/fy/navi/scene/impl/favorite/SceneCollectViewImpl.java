package com.fy.navi.scene.impl.favorite;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.favorite.ISceneCollectView;
import com.fy.navi.scene.api.search.ISceneQuickSearchView;
import com.fy.navi.scene.ui.favorite.SceneCollectView;
import com.fy.navi.ui.base.StackManager;

/**
 * 继承自BaseSceneModel，并封装了与搜索相关的操作，如关闭搜索页面、中止搜索等。
 */
public class SceneCollectViewImpl extends BaseSceneModel<SceneCollectView> implements ISceneCollectView {

    public SceneCollectViewImpl(SceneCollectView mScreenView) {
        super(mScreenView);
    }

    @Override
    public void closeSearch() {
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
    }
}

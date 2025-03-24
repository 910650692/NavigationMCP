package com.fy.navi.scene.impl.search;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.search.ISceneQuickSearchView;
import com.fy.navi.scene.ui.search.SceneQuickSearchView;
import com.fy.navi.ui.base.StackManager;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * 继承自BaseSceneModel，并封装了与搜索相关的操作，如关闭搜索页面、中止搜索等。
 */
public class SceneQuickSearchViewImpl extends BaseSceneModel<SceneQuickSearchView> implements ISceneQuickSearchView {

    public SceneQuickSearchViewImpl(final SceneQuickSearchView screenView) {
        super(screenView);
    }

    @Override
    public void closeSearch() {
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
    }
}

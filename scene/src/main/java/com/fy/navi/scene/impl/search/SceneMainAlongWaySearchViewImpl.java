package com.fy.navi.scene.impl.search;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.search.ISceneMainSearchView;
import com.fy.navi.scene.ui.search.SceneMainAlongWaySearchView;
import com.fy.navi.ui.base.StackManager;

/**
 * 继承自BaseSceneModel，并封装了与搜索相关的操作，如关闭搜索页面、中止搜索等。
 */
public class SceneMainAlongWaySearchViewImpl extends BaseSceneModel<SceneMainAlongWaySearchView> implements ISceneMainSearchView {

    /**
     * 构造方法，初始化父类和SearchPackage实例。
     *
     * @param mScreenView 搜索编辑栏视图对象
     */
    public SceneMainAlongWaySearchViewImpl(SceneMainAlongWaySearchView mScreenView) {
        super(mScreenView);
    }

    /**
     * 关闭搜索主页面。
     */
    @Override
    public void closeSearch() {
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
    }

    @Override
    public void onClickQuickSearch(int position) {
        mScreenView.onClickQuickSearch(position);
    }
}

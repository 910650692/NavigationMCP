package com.fy.navi.scene.impl.search;

import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.search.ISceneMainSearchView;
import com.fy.navi.scene.ui.search.SceneMainAlongWaySearchView;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.ui.base.StackManager;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * 继承自BaseSceneModel，并封装了与搜索相关的操作，如关闭搜索页面、中止搜索等。
 */
public class SceneMainAlongWaySearchViewImpl extends BaseSceneModel<SceneMainAlongWaySearchView> implements ISceneMainSearchView {

    private final LayerPackage layerPackage;

    /**
     * 构造方法，初始化父类和SearchPackage实例。
     *
     * @param screenView 搜索编辑栏视图对象
     */
    public SceneMainAlongWaySearchViewImpl(final SceneMainAlongWaySearchView screenView) {
        super(screenView);
        layerPackage = LayerPackage.getInstance();
    }

    /**
     * 关闭搜索主页面。
     */
    @Override
    public void closeSearch() {
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
    }

    @Override
    public void onClickQuickSearch(final int position) {
        mScreenView.onClickQuickSearch(position);
    }

    @Override
    public void onClickCollectSearch() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"ClickCollectSearch in SceneMainAlongWaySearchViewImpl");
    }

    public void flyLineVisible(MapType mapTypeId, boolean visible){
        layerPackage.openFlyLine(mapTypeId, visible);
    }
}

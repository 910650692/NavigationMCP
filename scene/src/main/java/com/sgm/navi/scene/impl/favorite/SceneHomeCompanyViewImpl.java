package com.sgm.navi.scene.impl.favorite;


import com.android.utils.log.Logger;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.api.favorite.ISceneHomeCompanyView;
import com.sgm.navi.scene.ui.favorite.SceneHomeCompanyView;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.ui.base.BaseFragment;
import com.sgm.navi.ui.base.StackManager;

/**
 * @author qlzou
 * @version \$Revision1.0\$
 * @Description: 家和公司
 * @CreateDate: $ $
 */
public class SceneHomeCompanyViewImpl extends BaseSceneModel<SceneHomeCompanyView> implements ISceneHomeCompanyView {
    private final SearchPackage mSearchPackage;
    private final LayerPackage mLayerPackage;
    private int mTaskId;

    /**
     * 获取高德SDK请求任务Id
     * @return 请求任务Id
     */
    public int getMTaskId() {
        return mTaskId;
    }
    public SceneHomeCompanyViewImpl(final SceneHomeCompanyView scrollView) {
        super(scrollView);
        mSearchPackage = SearchPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
    }

    @Override
    public void closeSearch() {
        BaseFragment baseFragment = StackManager.getInstance().getCurrentFragment(mMapTypeId.name());
        if (baseFragment != null) {
            baseFragment.closeFragment(true);
        }
    }

    @Override
    public void onClickQuickSearch(final int position) {
        mScreenView.onClickQuickSearch(position);
    }

    @Override
    public void onClickEditText() {
        mScreenView.onClickEditText();
    }

    /**
     * 预搜索
     * @param key 关键字
     */
    public void suggestionSearch(final String key) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "suggestionSearch  key:" + key);
        mTaskId = mSearchPackage.suggestionSearch(key);
    }

    public void currentLocationSearch() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "currentLocationSearch ");
        mTaskId = mSearchPackage.currentLocationSearch();
    }

    /**
     * 设置移图选点可见性
     * @param mapTypeId mapId
     * @param visible 是否可见
     */
    public void flyLineVisible(final MapType mapTypeId,final boolean visible){
        mLayerPackage.openFlyLine(mapTypeId, visible);
    }

}

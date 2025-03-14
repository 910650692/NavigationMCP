package com.fy.navi.scene.impl.favorite;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.text.TextUtils;

import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.favorite.ISceneHomeCompanyView;
import com.fy.navi.scene.api.favorite.ISceneMapPointSearchView;
import com.fy.navi.scene.ui.favorite.SceneHomeCompanyView;
import com.fy.navi.scene.ui.favorite.SceneMapPointSearchView;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.greendao.history.History;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.ui.base.StackManager;

import java.util.List;

/**
 * @Author: qlzou
 * @Description: 地图选点
 * @CreateDate: $ $
 */
public class SceneMapPointSearchViewImpl extends BaseSceneModel<SceneMapPointSearchView> implements ISceneMapPointSearchView {
    private final SearchPackage mSearchPackage;
    private final MapPackage mMapPackage;

    public SceneMapPointSearchViewImpl(SceneMapPointSearchView scrollView) {
        super(scrollView);
        mSearchPackage = SearchPackage.getInstance();
        mMapPackage = MapPackage.getInstance();
    }

    @Override
    public void closeFragment() {
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
        SceneMapPointSearchView.isMapPointSearchFragmentShow = false;
    }

    public void closeAllFragment(){
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeAllFragment();
        SceneMapPointSearchView.isMapPointSearchFragmentShow = false;
    }

    @Override
    public void clickSetting() {
        mScreenView.clickSetting();
    }

    public void doSearch(PoiInfoEntity poiInfoEntity) {
        if (null == poiInfoEntity) {
            Logger.d(SEARCH_HMI_TAG, "doSearch: currentLocationSearch");
            mSearchPackage.currentLocationSearch();
            mMapPackage.goToCarPosition(MapTypeId.MAIN_SCREEN_MAIN_MAP, true, true);
            return;
        }
        // 这里只有两种搜索类型：POI搜索和Geo搜索
        if (!TextUtils.isEmpty(poiInfoEntity.getPid())) {
            mSearchPackage.poiIdSearch(poiInfoEntity.getPid());
        } else {
            mSearchPackage.geoSearch(poiInfoEntity.getPoint());
        }
    }

}

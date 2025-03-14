package com.fy.navi.scene.impl.poi;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.poi.ISceneTitleBarView;
import com.fy.navi.scene.ui.poi.ScenePoiDetailTitleView;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.ui.base.StackManager;

/**
 * @Author: baipeng0904
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public class ScenePoiDetailTitleImpl extends BaseSceneModel<ScenePoiDetailTitleView> implements ISceneTitleBarView {
    private final SearchPackage mSearchPackage;

    public ScenePoiDetailTitleImpl(ScenePoiDetailTitleView mScreenView) {
        super(mScreenView);
        mSearchPackage = SearchPackage.getInstance();
    }

    @Override
    public void closeFragment() {
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
        // 清除扎标的点
        mSearchPackage.clearLabelMark();
    }

    @Override
    public void doSearch(PoiInfoEntity poiInfoEntity) {
        if (null == poiInfoEntity) {
            Logger.d(SEARCH_HMI_TAG, "doSearch: poiInfoEntity is null");
            return;
        }
        // 这里只有两种搜索类型：POI搜索和Geo搜索
        if (poiInfoEntity.getPoiType() == AutoMapConstant.SearchType.POI_SEARCH) {
            mSearchPackage.poiIdSearch(poiInfoEntity.getPid());
        } else {
            mSearchPackage.geoSearch(poiInfoEntity.getPoint());
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }
}

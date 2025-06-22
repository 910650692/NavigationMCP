package com.sgm.navi.scene.impl.poi;


import com.android.utils.log.Logger;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.api.poi.ISceneTitleBarView;
import com.sgm.navi.scene.ui.poi.ScenePoiDetailTitleView;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.ui.base.StackManager;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 *
 */
public class ScenePoiDetailTitleImpl extends BaseSceneModel<ScenePoiDetailTitleView> implements ISceneTitleBarView {
    private final SearchPackage mSearchPackage;

    public ScenePoiDetailTitleImpl(final ScenePoiDetailTitleView screenView) {
        super(screenView);
        mSearchPackage = SearchPackage.getInstance();
    }

    @Override
    public void closeFragment() {
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
        // 清除扎标的点
        mSearchPackage.clearLabelMark();
    }

    @Override
    public void doSearch(final PoiInfoEntity poiInfoEntity) {
        if (null == poiInfoEntity) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "doSearch: poiInfoEntity is null");
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

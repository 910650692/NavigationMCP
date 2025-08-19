package com.sgm.navi.scene.impl.search;

import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.api.search.ISceneSearchHistory;
import com.sgm.navi.scene.ui.search.SceneSearchHistoryView;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.greendao.history.History;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.user.behavior.BehaviorPackage;

import java.util.List;


public class SceneMainAlongWaySearchHistoryImpl extends BaseSceneModel<SceneSearchHistoryView> implements ISceneSearchHistory {
    private final SearchPackage mSearchPackage;
    private final BehaviorPackage mBehaviorPackage;

    public SceneMainAlongWaySearchHistoryImpl(final SceneSearchHistoryView screenView) {
        super(screenView);
        mSearchPackage = SearchPackage.getInstance();
        mBehaviorPackage = BehaviorPackage.getInstance();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    @Override
    public void getSearchKeywordRecord() {
        final List<History> historyList = mSearchPackage.getNaviRecord();
        mScreenView.notifyKeywordRecord(historyList);
    }

    /**
     * 是否收藏
     *
     * @param poiInfo PoiInfoEntity
     * @return itemId 已经收藏 null 未收藏
     */
    public String isFavorite(final PoiInfoEntity poiInfo) {
        return mBehaviorPackage.isHomeOrCompanyOrFavorite(poiInfo);
    }

    /**
     * 是否常去地址
     *
     * @param poiInfo PoiInfoEntity
     * @return true 是 否 不是
     */
    public boolean isFrequentAddress(final PoiInfoEntity poiInfo) {
        return mBehaviorPackage.isFrequentAddress(poiInfo);
    }

    @Override
    public void clearSearchKeywordRecord() {
        mSearchPackage.clearSearchKeywordRecord();
    }
}

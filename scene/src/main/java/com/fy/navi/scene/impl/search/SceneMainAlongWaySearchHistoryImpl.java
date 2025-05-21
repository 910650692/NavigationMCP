package com.fy.navi.scene.impl.search;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.search.ISceneSearchHistory;
import com.fy.navi.scene.ui.search.SceneSearchHistoryView;
import com.fy.navi.service.greendao.history.History;
import com.fy.navi.service.logicpaket.search.SearchPackage;

import java.util.List;


public class SceneMainAlongWaySearchHistoryImpl extends BaseSceneModel<SceneSearchHistoryView> implements ISceneSearchHistory {
    private final SearchPackage mSearchPackage;

    public SceneMainAlongWaySearchHistoryImpl(final SceneSearchHistoryView screenView) {
        super(screenView);
        mSearchPackage = SearchPackage.getInstance();
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

    @Override
    public void clearSearchKeywordRecord() {
        mSearchPackage.clearSearchKeywordRecord();
    }
}

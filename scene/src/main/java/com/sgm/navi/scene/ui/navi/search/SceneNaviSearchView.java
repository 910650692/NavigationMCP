package com.sgm.navi.scene.ui.navi.search;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.SceneNaviSearchViewBinding;
import com.sgm.navi.scene.impl.navi.inter.ISceneCallback;
import com.sgm.navi.scene.impl.navi.search.SceneNaviSearchViewImpl;
import com.sgm.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneBase;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneManager;
import com.sgm.navi.service.define.search.SearchResultEntity;

public class SceneNaviSearchView extends NaviSceneBase<SceneNaviSearchViewBinding, SceneNaviSearchViewImpl> {

    public static final String TAG = "SceneNaviSearchView";

    public SceneNaviSearchView(@NonNull Context context) {
        super(context);
    }

    public SceneNaviSearchView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviSearchView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return null;
    }

    @Override
    public String getSceneName() {
        return "";
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    @Override
    protected void init() {

    }

    @Override
    public void addSceneCallback(ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
        mScreenViewModel.addISceneCallback(sceneCallback);
    }

    @Override
    protected SceneNaviSearchViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneNaviSearchViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviSearchViewImpl initSceneImpl() {
        return new SceneNaviSearchViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviSearch(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        defaultDataProcessing();
        mViewBinding.naviAlongWayList.setSceneNaviSearchView(mScreenViewModel);
        mViewBinding.naviSearchResult.setSceneNaviSearchView(mScreenViewModel);
    }

    /**
     * 默认数据处理
     */
    private void defaultDataProcessing() {
        final int powerType = mScreenViewModel.powerType();
        final String[] categories;
        final TypedArray iconArray;
        // 油车
        if (powerType == 0) {
            categories = getResources().getStringArray(R.array.along_way_categories_gas_name);
            iconArray = getResources().obtainTypedArray(R.array.along_way_category_gas_icons);
        }else {
            categories = getResources().getStringArray(R.array.along_way_categories_charging_name);
            iconArray = getResources().obtainTypedArray(R.array.along_way_category_charging_icons);
        }
        Logger.i(TAG, "defaultDataProcessing: ");
        mViewBinding.naviAlongWayList.setQuickSearchListAdapterData(iconArray, categories);
    }

    public void goSearchView(String keyWord, int searchType) {
        mScreenViewModel.goSearchView(keyWord, searchType);
    }

    public void goAlongWayList() {
        mScreenViewModel.goAlongWayList();
    }

    public void setSearchResultData(final SearchResultEntity searchResultEntity) {
        mViewBinding.naviSearchResult.setSearchResultData(searchResultEntity);
    }

    public void closeSearchResultView() {

    }
}

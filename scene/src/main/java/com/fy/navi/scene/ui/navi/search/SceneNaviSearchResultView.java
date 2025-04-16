package com.fy.navi.scene.ui.navi.search;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.api.navi.ISceneNaviSearchView;
import com.fy.navi.scene.databinding.SceneNaviSearchResultViewBinding;
import com.fy.navi.scene.impl.navi.search.SceneNaviSearchResultViewImpl;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.navi.OpenApiHelper;

import java.util.List;

public class SceneNaviSearchResultView extends BaseSceneView<SceneNaviSearchResultViewBinding,
        SceneNaviSearchResultViewImpl> {

    private ISceneNaviSearchView mISceneNaviSearchView;

    public SceneNaviSearchResultView(@NonNull Context context, @Nullable AttributeSet attrs,
                                     int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public SceneNaviSearchResultView(@NonNull Context context,
                                     @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviSearchResultView(@NonNull Context context) {
        super(context);
    }

    @Override
    protected SceneNaviSearchResultViewBinding createViewBinding(LayoutInflater inflater,
                                                                 ViewGroup viewGroup) {
        return SceneNaviSearchResultViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviSearchResultViewImpl initSceneImpl() {
        return new SceneNaviSearchResultViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviSearch(mScreenViewModel);
        mViewBinding.naviSearchType.setNaviSearch(mScreenViewModel);
    }

    @Override
    protected void initObserver() {

    }

    public void setSearchResultData(final SearchResultEntity searchResultEntity) {
        mScreenViewModel.updateData(searchResultEntity);
    }

    /**
     * 更新充电站列表
     * @param poiList poiList
     * @param allPoiParamList allPoiParamList
     * @param searchType searchType
     */
    public void showNaviSearchChargeListUI(List<PoiInfoEntity> poiList,
                                           List<RouteParam> allPoiParamList, int searchType) {
        updateSearchTypeView(searchType);
        mViewBinding.chargeStationList.notifyResultList(poiList, allPoiParamList, searchType,0);
    }

    /**
     * 更新其他列表
     * @param poiInfoEntityList poiInfoEntityList
     */
    public void showNaviOtherListUI(List<PoiInfoEntity> poiInfoEntityList) {
        updateSearchTypeView(OpenApiHelper.getSearchType());
        mViewBinding.otherList.notifyResultList(poiInfoEntityList);
    }

    public void setSceneNaviSearchView(ISceneNaviSearchView sceneNaviSearchView) {
        mISceneNaviSearchView = sceneNaviSearchView;
    }

    public void closeSearchResultView() {
        mISceneNaviSearchView.closeSearchResultView();
    }

    public void onSearTypeClick(String keyword, int type) {
        updateSearchTypeView(type);
        mISceneNaviSearchView.startSearch(keyword, type);
    }

    private void updateSearchTypeView(int type) {
        mViewBinding.naviSearchType.sivAlongWay.setSelected(type == 0);
        mViewBinding.naviSearchType.stvAlongWay.setSelected(type == 0);
        mViewBinding.naviSearchType.sivDestination.setSelected(type == 1);
        mViewBinding.naviSearchType.stvDestination.setSelected(type == 1);
        mViewBinding.naviSearchType.sivAround.setSelected(type == 2);
        mViewBinding.naviSearchType.stvAround.setSelected(type == 2);
    }
}

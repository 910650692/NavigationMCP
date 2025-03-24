package com.fy.navi.hmi.search.searchresult;


import android.os.Bundle;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentSearchResultBinding;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.base.BaseFragment;

@Route(path = RoutePath.Search.SEARCH_RESULT_FRAGMENT)
public class SearchResultFragment extends BaseFragment<FragmentSearchResultBinding, SearchResultViewModel> {
    private String mSourceFragmentTag;
    @Override
    public int onLayoutId() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onLayoutId");
        return R.layout.fragment_search_result;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onInitView");
        mBinding.scenePoiList.setScreenId(MapTypeId.valueOf(mScreenId));
    }

    @Override
    public void onInitData() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onInitData");
        getBundleData();
    }

    /**
     * 获取bundle数据
     */
    private void getBundleData() {
        final Bundle parsedArgs = getArguments();
        if (parsedArgs == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "No valid arguments found.");
            return;
        }

        mSourceFragmentTag = parsedArgs.getString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT);
        final int searchType = parsedArgs.getInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE);
        final String keyword = parsedArgs.getString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD);
        final PoiInfoEntity entity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_POI_LIST);
        mBinding.scenePoiList.setPoiInfoEntity(entity);
        mBinding.scenePoiList.setEditText(searchType, keyword);
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "sourceFragmentTag : " + mSourceFragmentTag);
    }

    /**
     * 搜索结果回调
     * @param searchResultEntity 搜索结果实体类
     */
    public void notifySearchResult(final SearchResultEntity searchResultEntity) {
        int homeCompanyType = -1;
        if (ConvertUtils.equals(mSourceFragmentTag, AutoMapConstant.SourceFragment.FRAGMENT_HOME)) {
            homeCompanyType = AutoMapConstant.HomeCompanyType.HOME;
        } else if (ConvertUtils.equals(mSourceFragmentTag, AutoMapConstant.SourceFragment.FRAGMENT_COMPANY)) {
            homeCompanyType = AutoMapConstant.HomeCompanyType.COMPANY;
        } else if (ConvertUtils.equals(mSourceFragmentTag, AutoMapConstant.SourceFragment.FRAGMENT_COLLECTION)) {
            homeCompanyType = AutoMapConstant.HomeCompanyType.COLLECTION;
        } else if (ConvertUtils.equals(mSourceFragmentTag, AutoMapConstant.SourceFragment.FRAGMENT_COMMON)) {
            homeCompanyType = AutoMapConstant.HomeCompanyType.COMMON;
        }
        mBinding.scenePoiList.setHomeCompanyState(homeCompanyType);
        mBinding.scenePoiList.notifySearchResult(searchResultEntity);
    }

    /**
     * 静默搜索回调
     * @param searchResultEntity 搜索回调实体类
     */
    public void notifySilentSearchResult(final SearchResultEntity searchResultEntity) {
        mBinding.scenePoiList.notifySilentSearchResult(searchResultEntity);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mBinding.scenePoiList.clear();
    }
}

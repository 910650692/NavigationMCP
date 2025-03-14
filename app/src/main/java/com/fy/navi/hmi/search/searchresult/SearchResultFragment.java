package com.fy.navi.hmi.search.searchresult;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.os.Bundle;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentSearchResultBinding;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.base.BaseFragment;

@Route(path = RoutePath.Search.SEARCH_RESULT_FRAGMENT)
public class SearchResultFragment extends BaseFragment<FragmentSearchResultBinding, SearchResultViewModel> {
    private String sourceFragmentTag;
    @Override
    public int onLayoutId() {
        Logger.d(SEARCH_HMI_TAG, "onLayoutId");
        return R.layout.fragment_search_result;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        Logger.d(SEARCH_HMI_TAG, "onInitView");
        mBinding.scenePoiList.setScreenId(MapTypeId.valueOf(mScreenId));
    }

    @Override
    public void onInitData() {
        Logger.d(SEARCH_HMI_TAG, "onInitData");
        getBundleData();
    }

    private void getBundleData() {
        Bundle parsedArgs = getArguments();
        if (parsedArgs == null) {
            Logger.d(SEARCH_HMI_TAG, "No valid arguments found.");
            return;
        }

        sourceFragmentTag = parsedArgs.getString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT);
        int searchType = parsedArgs.getInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE);
        String keyword = parsedArgs.getString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD);
        PoiInfoEntity entity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_POI_LIST);
        mBinding.scenePoiList.setPoiInfoEntity(entity);
        mBinding.scenePoiList.setEditText(searchType, keyword);
        Logger.d(SEARCH_HMI_TAG, "sourceFragmentTag : " + sourceFragmentTag);
    }

    public void notifySearchResult(SearchResultEntity searchResultEntity) {
        int homeCompanyType = -1;
        if (ConvertUtils.equals(sourceFragmentTag, AutoMapConstant.SourceFragment.FRAGMENT_HOME)) {
            homeCompanyType = AutoMapConstant.HomeCompanyType.HOME;
        } else if (ConvertUtils.equals(sourceFragmentTag, AutoMapConstant.SourceFragment.FRAGMENT_COMPANY)) {
            homeCompanyType = AutoMapConstant.HomeCompanyType.COMPANY;
        } else if (ConvertUtils.equals(sourceFragmentTag, AutoMapConstant.SourceFragment.FRAGMENT_COLLECTION)) {
            homeCompanyType = AutoMapConstant.HomeCompanyType.COLLECTION;
        } else if (ConvertUtils.equals(sourceFragmentTag, AutoMapConstant.SourceFragment.FRAGMENT_COMMON)) {
            homeCompanyType = AutoMapConstant.HomeCompanyType.COMMON;
        }
        mBinding.scenePoiList.setHomeCompanyState(homeCompanyType);
        mBinding.scenePoiList.notifySearchResult(searchResultEntity);
    }

    public void notifySilentSearchResult(SearchResultEntity searchResultEntity) {
        mBinding.scenePoiList.notifySilentSearchResult(searchResultEntity);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mBinding.scenePoiList.clear();
    }
}

package com.fy.navi.hmi.favorite;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.os.Bundle;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentHomeCompanyBinding;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.api.search.IOnHomeCompanyClickListener;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.base.BaseFragment;

/**
 * 家、公司、常用地址、收藏
 */
@Route(path = RoutePath.Search.HOME_COMPANY_FRAGMENT)
public class HomeCompanyFragment extends BaseFragment<FragmentHomeCompanyBinding, HomeCompanyViewModel> {
    private int homeCompany = -1;
    @Override
    public int onLayoutId() {
        return R.layout.fragment_home_company;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        mBinding.homeCompanyView.setScreenId(MapTypeId.valueOf(mScreenId));
    }

    @Override
    public void onInitData() {
        defaultDataProcessing();
        mBinding.homeCompanyView.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.sceneNestedScrollView.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.sceneNestedScrollView.setHomeCompanyState(homeCompany);
        mBinding.homeCompanyView.setClickListener(new IOnHomeCompanyClickListener() {
            @Override
            public void onEditClearClicked() {
                //清除预搜索关键字后默认展示历史记录
                mBinding.sceneNestedScrollView.getSearchKeywordRecord();
            }

            @Override
            public void setHomeCompanyType(int type) {
                FavoriteHelper.getInstance().setHomeCompanyType(type);
            }
        });
    }

    private void defaultDataProcessing() {
        Bundle parsedArgs = getArguments();
        if (parsedArgs == null) {
            Logger.d(SEARCH_HMI_TAG, "No valid arguments found.");
            return;
        }
        String sourceFragmentTag = parsedArgs.getString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT);
        int searchType = parsedArgs.getInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, 0);
        // 1:家 2:公司 3:常用地址 0:收藏夹
        homeCompany = parsedArgs.getInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, -1);
        Logger.d(SEARCH_HMI_TAG, "searchType:" + searchType + "homCompany:" + homeCompany);
        mBinding.homeCompanyView.setViewVisibility(homeCompany);
        mViewModel.setHomeCompanyType(homeCompany);
    }

    public void notifySearchResult(SearchResultEntity searchResultEntity) {
        if (searchResultEntity != null) {
            Logger.d(SEARCH_HMI_TAG, "searchType:" + searchResultEntity.getSearchType());
            if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.GEO_SEARCH) {
                mBinding.homeCompanyView.notifySearchResult(searchResultEntity);
            } else {
                mBinding.sceneNestedScrollView.setHomeCompanyState(homeCompany);
                mBinding.sceneNestedScrollView.notifySearchResult(searchResultEntity);
            }
        }

    }

    public void notifySilentSearchResult(SearchResultEntity searchResultEntity) {
        mBinding.sceneNestedScrollView.notifySilentSearchResult(searchResultEntity);
    }

    @Override
    public void onHiddenChanged(boolean hidden) {
        super.onHiddenChanged(hidden);
        if (!hidden) {
            mBinding.sceneNestedScrollView.getSearchKeywordRecord();
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }
}

package com.sgm.navi.hmi.favorite;


import android.os.Bundle;
import android.view.View;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentHomeCompanyBinding;
import com.sgm.navi.scene.RoutePath;
import com.sgm.navi.scene.api.search.IOnHomeCompanyClickListener;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.ui.base.BaseFragment;

@Route(path = RoutePath.Search.HOME_COMPANY_FRAGMENT)
public class HomeCompanyFragment extends BaseFragment<FragmentHomeCompanyBinding, HomeCompanyViewModel> {
    private int mHomeCompany = -1;
    private String mKeyword = "";
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
        mBinding.homeCompanyView.setScreenId(MapType.valueOf(mScreenId));
    }

    @Override
    public void onInitData() {
        defaultDataProcessing();
        mBinding.homeCompanyView.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNestedScrollView.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNestedScrollView.setHomeCompanyState(mHomeCompany);
        mBinding.homeCompanyView.setClickListener(new IOnHomeCompanyClickListener() {
            @Override
            public void onEditClearClicked() {
                //清除预搜索关键字后默认展示历史记录
                mBinding.sceneNestedScrollView.getSearchKeywordRecord();
            }

            @Override
            public void setHomeCompanyType(final int type) {
                FavoriteHelper.getInstance().setHomeCompanyType(type);
                mBinding.getRoot().setVisibility(View.GONE);
            }
        });
    }

    /**
     * 处理Bundle传递的数据
     */
    private void defaultDataProcessing() {
        final Bundle parsedArgs = getArguments();
        if (parsedArgs == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "No valid arguments found.");
            return;
        }
        final String sourceFragmentTag = parsedArgs.getString(
                AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT);
        mKeyword = parsedArgs.getString(AutoMapConstant.VoiceKeyWord.BUNDLE_VOICE_KEY_WORD);
        final int searchType = parsedArgs.getInt(
                AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, 0);
        // 1:家 2:公司 3:常用地址 0:收藏夹
        mHomeCompany = parsedArgs.getInt(
            AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, -1);
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "searchType:" + searchType + "homCompany:" +
            mHomeCompany + "sourceFragmentTag:" + sourceFragmentTag + "keyword:" + mKeyword);
        mBinding.homeCompanyView.setViewVisibility(mHomeCompany);
        if (!ConvertUtils.isEmpty(mKeyword)) {
            mBinding.homeCompanyView.doKeyWordSearch(mKeyword);
        }
        mViewModel.setHomeCompanyType(mHomeCompany);
    }

    /**
     * 高德SDK数据回调
     * @param taskId 任务id
     * @param searchResultEntity 封装的数据实体类
     */
    public void notifySearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        if (searchResultEntity != null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "searchType:" + searchResultEntity.getSearchType());
            mBinding.homeCompanyView.notifySearchResult(taskId, searchResultEntity);
            mBinding.sceneNestedScrollView.setHomeCompanyState(mHomeCompany);
            mBinding.sceneNestedScrollView.notifySearchResult(searchResultEntity);
        }

    }

    /**
     * notifySilentSearchResult
     * @param searchResultEntity
     */
    public void notifySilentSearchResult(final SearchResultEntity searchResultEntity) {
        mBinding.sceneNestedScrollView.notifySilentSearchResult(searchResultEntity);
    }

    @Override
    public void onHiddenChanged(final boolean hidden) {
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

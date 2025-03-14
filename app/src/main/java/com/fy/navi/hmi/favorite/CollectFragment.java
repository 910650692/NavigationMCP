package com.fy.navi.hmi.favorite;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.os.Bundle;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentCollectBinding;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.ui.base.BaseFragment;

import java.util.List;

/**
 * 收藏页面
 */
@Route(path = RoutePath.Search.COLLECT_FRAGMENT)
public class CollectFragment extends BaseFragment<FragmentCollectBinding, CollectViewModel> {
    //0 普通收藏 1常用地址 2收到的点
    private int collectionType;

    @Override
    public int onLayoutId() {
        return R.layout.fragment_collect;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        mBinding.collectView.setScreenId(MapTypeId.valueOf(mScreenId));
    }

    @Override
    public void onInitData() {
        defaultDataProcessing();
    }

    /**
     * 获取收藏列表
     */
    private void initFavoriteList() {
        List<PoiInfoEntity> poiInfoEntityList = mViewModel.getFavoriteListAsync();
        ThreadManager.getInstance().postUi(() -> {
            mBinding.collectView.setAdapterData(poiInfoEntityList);
        });
    }

    private void initPushMsgList() {
        List<PoiInfoEntity> poiInfoEntityList = mViewModel.getPushMsgList();
        ThreadManager.getInstance().postUi(() -> {
            mBinding.collectView.setAdapterData(poiInfoEntityList);
        });
    }

    private void defaultDataProcessing() {
        Bundle parsedArgs = getArguments();
        if (parsedArgs == null) {
            Logger.d(SEARCH_HMI_TAG, "No valid arguments found.");
            return;
        }

        String sourceFragmentTag = parsedArgs.getString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT);
        collectionType = parsedArgs.getInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_COLLECTION);
        int homeCompanyType = parsedArgs.getInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, 0);
        mBinding.collectView.setCollectionType(collectionType);
        mBinding.collectView.setHomeCompanyType(homeCompanyType);
        Logger.d(SEARCH_HMI_TAG, "collectionType: " + collectionType);
        if (collectionType == AutoMapConstant.CollectionType.GET_POINT) {
            initPushMsgList();
        } else {
            initFavoriteList();
        }
    }

    @Override
    public void onHiddenChanged(boolean hidden) {
        super.onHiddenChanged(hidden);
        if (!hidden) {
            initFavoriteList();
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }
    public void setAdapterData(){
        ThreadManager.getInstance().postUi(() -> {
            List<PoiInfoEntity> poiInfoEntityList = mViewModel.getFavoriteListAsync();
            mBinding.collectView.setAdapterData(poiInfoEntityList);
        });
    }
}

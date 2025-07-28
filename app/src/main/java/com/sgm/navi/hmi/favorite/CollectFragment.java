package com.sgm.navi.hmi.favorite;

import android.os.Bundle;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentCollectBinding;
import com.sgm.navi.scene.RoutePath;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.ui.base.BaseFragment;

import java.util.ArrayList;
import java.util.List;

@Route(path = RoutePath.Search.COLLECT_FRAGMENT)
public class CollectFragment extends BaseFragment<FragmentCollectBinding, CollectViewModel> {
    //0 普通收藏 1常用地址 2收到的点 4途径点
    private int mCollectionType;

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
        mBinding.collectView.setScreenId(MapType.valueOf(mScreenId));
    }

    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_FAVORITE_LIST)
    public void onInitData() {
        defaultDataProcessing();
        mBinding.collectView.setPowerType(mViewModel.powerType());
    }

    /**
     * 获取收藏列表
     */
    private void initFavoriteList() {
        ThreadManager.getInstance().runAsync(() -> {
            final List<PoiInfoEntity> poiInfoEntityList = mViewModel.getFavoriteListAsync();
            ThreadManager.getInstance().postUi(() -> {
                mBinding.collectView.setAdapterData(poiInfoEntityList,false);
            });
        });
    }

    /**
     * initPushMsgList
     */
    private void initPushMsgList() {
        final List<PoiInfoEntity> poiInfoEntityList = mViewModel.getPushMsgList();
        ThreadManager.getInstance().postUi(() -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "initPushMsgList size: " + poiInfoEntityList.size());
            mBinding.collectView.setAdapterData(poiInfoEntityList,false);
        });
    }

    /**
     * defaultDataProcessing
     */
    private void defaultDataProcessing() {
        final Bundle parsedArgs = getArguments();
        if (parsedArgs == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "No valid arguments found.");
            return;
        }

        mCollectionType = parsedArgs.getInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_COLLECTION);
        final int homeCompanyType = parsedArgs.getInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, 0);
        mBinding.collectView.setCollectionType(mCollectionType);
        mBinding.collectView.setHomeCompanyType(homeCompanyType);
        mBinding.collectView.setSourceFragment(parsedArgs.getString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT));
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "collectionType: " + mCollectionType);
        if (mCollectionType == AutoMapConstant.CollectionType.GET_POINT) {
            initPushMsgList();
        } else {
            initFavoriteList();
        }
    }

    @Override
    public void onHiddenChanged(final boolean hidden) {
        super.onHiddenChanged(hidden);
        if (!hidden) {
            mBinding.collectView.setChargeType();
            if (mCollectionType == AutoMapConstant.CollectionType.GET_POINT) {
                initPushMsgList();
            } else {
                initFavoriteList();
            }
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    /**
     * setAdapterData
     */
    public void setAdapterData(){
        ThreadManager.getInstance().postUi(() -> {
            final List<PoiInfoEntity> poiInfoEntityList = mViewModel.getFavoriteListAsync();
            mBinding.collectView.setAdapterData(poiInfoEntityList,false);
        });
    }

    public void notifyNetSearchResult(int taskId,ArrayList<PoiInfoEntity> poiInfoEntity){
        mBinding.collectView.notifyNetSearchResult(taskId,poiInfoEntity);
    }

    public void notifySearchResultByNetError(int taskId,String message){
        mBinding.collectView.notifySearchResultByNetError(taskId,message);
    }

    @Override
    protected void onBackPressed() {
        mBinding.collectView.onBackPressed();
    }
}

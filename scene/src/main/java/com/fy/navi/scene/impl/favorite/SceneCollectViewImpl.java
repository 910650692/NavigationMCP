package com.fy.navi.scene.impl.favorite;

import androidx.lifecycle.MutableLiveData;

import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.favorite.ISceneCollectView;
import com.fy.navi.scene.ui.favorite.SceneCollectView;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.ui.base.StackManager;

import java.util.ArrayList;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * 继承自BaseSceneModel，并封装了与搜索相关的操作，如关闭搜索页面、中止搜索等。
 */
public class SceneCollectViewImpl extends BaseSceneModel<SceneCollectView> implements ISceneCollectView {
    // 动力类型标定
    public MutableLiveData<Integer> mPowerType = new MutableLiveData<>();
    public MutableLiveData<Boolean> mChargingVisibility;
    public MutableLiveData<Boolean> mChargingNoDataVisibility;
    public MutableLiveData<Boolean> mChargingRequestFailedVisibility;
    public MutableLiveData<Boolean> mChargingOfflineVisibility;
    public MutableLiveData<Boolean> mTipVisibility;
    public SceneCollectViewImpl(final SceneCollectView screenView) {
        super(screenView);
        mPowerType = new MutableLiveData<>(-1);
        mChargingVisibility = new MutableLiveData<>(false);
        mChargingNoDataVisibility = new MutableLiveData<>(false);
        mChargingRequestFailedVisibility = new MutableLiveData<>(false);
        mChargingOfflineVisibility = new MutableLiveData<>(false);
        mTipVisibility = new MutableLiveData<>(false);
    }

    @Override
    public void closeSearch() {
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
    }

    // 判断SGM是否已登陆
    public boolean isSGMLogin(){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"isSGMLogin: "+AccountPackage.getInstance().isSGMLogin());
        return AccountPackage.getInstance().isSGMLogin();
    }

    public ArrayList<PoiInfoEntity> getFavoriteListAsync() {
        return BehaviorPackage.getInstance().getFavoritePoiData();
    }

    public void startSGMLogin(){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"startSGMLogin");
        AccountPackage.getInstance().sendSGMLoginRequest(mScreenView.getContext());
    }
}

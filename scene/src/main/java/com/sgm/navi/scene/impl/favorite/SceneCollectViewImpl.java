package com.sgm.navi.scene.impl.favorite;

import android.content.Context;

import androidx.lifecycle.MutableLiveData;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.api.favorite.ISceneCollectView;
import com.sgm.navi.scene.ui.favorite.SceneCollectView;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.user.account.AccessTokenParam;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.user.account.AccountPackage;
import com.sgm.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.sgm.navi.ui.base.StackManager;

import java.util.ArrayList;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * 继承自BaseSceneModel，并封装了与搜索相关的操作，如关闭搜索页面、中止搜索等。
 */
public class SceneCollectViewImpl extends BaseSceneModel<SceneCollectView> implements ISceneCollectView {
    // 动力类型标定
    public MutableLiveData<Integer> mPowerType = new MutableLiveData<>();
    private final SearchPackage mSearchPackage;
    private int mTaskId;
    public int getMTaskId() {
        return mTaskId;
    }
    public SceneCollectViewImpl(final SceneCollectView screenView) {
        super(screenView);
        mPowerType = new MutableLiveData<>(-1);
        mSearchPackage = SearchPackage.getInstance();
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
        ArrayList<PoiInfoEntity> list = BehaviorPackage.getInstance().getFavoritePoiData();
        if (ConvertUtils.isNull(list)) {
            return new ArrayList<>();
        }
        // 过滤掉无详细地址的收藏info
//        list.removeIf(poiInfo -> ConvertUtils.isEmpty(poiInfo.getAddress()));
        return list;
    }

    public void startSGMLogin(){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"startSGMLogin");
        if (ConvertUtils.isNull(mScreenView)) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "ScreenView is null, cannot start SGM login");
            return;
        }

        Context context = mScreenView.getContext();
        if (ConvertUtils.isNull(context)) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Context is null, cannot start SGM login");
            return;
        }

        try {
            AccountPackage.getInstance().sendSGMLoginRequest(context);
        } catch (Exception e) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "SGM login request failed", e);
        }
    }

    public void queryCollectStation(AccessTokenParam param){
        ThreadManager.getInstance().runAsync(() -> {
            String idpUserId = AccountPackage.getInstance().getUserId();
            String accessToken = AccountPackage.getInstance().getAccessToken(param);
            String vehicleBrand = mSearchPackage.getBrandName(CalibrationPackage.getInstance().brand());
            mTaskId = mSearchPackage.queryCollectStation(idpUserId,accessToken,vehicleBrand);
        });
    }

    public void updateCollectStatus(AccessTokenParam param,PoiInfoEntity poiInfoEntity){
        ThreadManager.getInstance().runAsync(() -> {
            String idpUserId = AccountPackage.getInstance().getUserId();
            String accessToken = AccountPackage.getInstance().getAccessToken(param);
            String vehicleBrand = mSearchPackage.getBrandName(CalibrationPackage.getInstance().brand());
            if(!ConvertUtils.isNull(poiInfoEntity)){
                poiInfoEntity.setIsCollect(true);
            }
            mTaskId = mSearchPackage.updateCollectStatus(idpUserId,accessToken,vehicleBrand,poiInfoEntity);
        });
    }
}

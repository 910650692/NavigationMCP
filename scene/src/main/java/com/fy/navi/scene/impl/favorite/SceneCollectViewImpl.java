package com.fy.navi.scene.impl.favorite;

import android.app.Activity;
import android.content.Context;

import androidx.lifecycle.MutableLiveData;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.favorite.ISceneCollectView;
import com.fy.navi.scene.ui.favorite.SceneCollectView;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.user.account.AccessTokenParam;
import com.fy.navi.service.logicpaket.search.SearchPackage;
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
    private final SearchPackage mSearchPackage;
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
        return BehaviorPackage.getInstance().getFavoritePoiData();
    }

    public void startSGMLogin(){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"startSGMLogin");
        AccountPackage.getInstance().sendSGMLoginRequest(mScreenView.getContext());
    }

    public void queryCollectStation(Activity context){
        AccessTokenParam param = new AccessTokenParam(
                AutoMapConstant.AccountTokenParamType.ACCOUNT_TYPE_PATAC_HMI,
                AutoMapConstant.AccountTokenParamType.AUTH_TOKEN_TYPE_READ_ONLY,
                null,
                context,
                null,
                null,
                null,
                null);

        ThreadManager.getInstance().runAsync(() -> {
            String idpUserId = AccountPackage.getInstance().getUserId();
            String accessToken = AccountPackage.getInstance().getAccessToken(param);
            String vehicleBrand = "BUICK";
            mSearchPackage.queryCollectStation(idpUserId,accessToken,vehicleBrand);
        });
    }
}

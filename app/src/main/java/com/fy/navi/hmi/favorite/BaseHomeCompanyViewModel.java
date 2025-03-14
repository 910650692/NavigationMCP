package com.fy.navi.hmi.favorite;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseHomeCompanyViewModel extends BaseViewModel<HomeCompanyFragment, HomeCompanyModel>  {

    public BaseHomeCompanyViewModel(@NonNull Application application) {
        super(application);
    }
    private int homeCompanyType;

    @Override
    protected HomeCompanyModel initModel() {
        return new HomeCompanyModel();
    }

    public Action rootClick = () -> {
    };

    public void setHomeCompanyType(int homeCompanyType) {
        // 1:家 2:公司 3:常用地址 0:收藏夹
        this.homeCompanyType = homeCompanyType;
    }

    public void notifySearchResult(SearchResultEntity searchResultEntity) {
        mView.notifySearchResult(searchResultEntity);
    }

    public void notifySilentSearchResult(SearchResultEntity searchResultEntity) {
        mView.notifySilentSearchResult(searchResultEntity);
    }


    /**
     * 点击搜索item
     *
     * @param position
     * @param poiInfoEntity
     */
   /* @Override
    public void onItemClick(int position, PoiInfoEntity poiInfoEntity) {
       *//* Bundle bundle = new Bundle();
        bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL, poiInfoEntity);
        bundle.putInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, mViewModel.getPoiTypeByHomeCompany());
        addFragment(new PoiDetailsFragment(), bundle);*//*
    }*/

    /**
     * 点击item上的添加按钮
     *
     * @param position
     * @param poiInfoEntity
     */
   /* @Override
    public void onNaviClick(int position, PoiInfoEntity poiInfoEntity) {
    *//*    //添加家/公司 提示并执行记录操作
        ToastUtils.Companion.getInstance().showText(ResourceUtils.Companion.getInstance().getString(R.string.hcm_set_success), 0);
        int commonName = mViewModel.getFavoriteCommonNameByHomeCompany();
        FavoriteInfo favoriteInfo = new FavoriteInfo();
        favoriteInfo.setCommonName(commonName)
                .setItemId(poiInfoEntity.getPid() + "_" + poiInfoEntity.getName() + "_" + poiInfoEntity.getPoint().getLon() + "_" + poiInfoEntity.getPoint().getLat())
                .setUpdateTime(new Date().getTime());
        poiInfoEntity.setFavoriteInfo(favoriteInfo);
        BehaviorPackage.getInstance().addFavoriteData(poiInfoEntity, commonName);
        SettingUpdateObservable.getInstance().onUpdateSyncTime(*//*);
        closeFragment(true);
    }*/
}

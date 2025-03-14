package com.fy.navi.scene.ui.favorite;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.SceneMapPointSearchViewBinding;
import com.fy.navi.scene.impl.favorite.SceneMapPointSearchViewImpl;
import com.fy.navi.scene.ui.search.SearchLoadingDialog;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.AutoMapConstant.HomeCompanyType;
import com.fy.navi.service.AutoMapConstant.PoiType;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;

import java.util.Date;

/**
 * @Author: qlzou
 * @Description: 地图选点
 * @CreateDate: $ $
 */
public class SceneMapPointSearchView extends BaseSceneView<SceneMapPointSearchViewBinding, SceneMapPointSearchViewImpl> {

    public static boolean isMapPointSearchFragmentShow = false;
    @PoiType
    public static int poiType;
    private String hintText = "";
    private SearchLoadingDialog searchLoadingDialog;
    private PoiInfoEntity poiInfoEntity;
    //common_name：1，家  2，公司 3.常用地址  0，普通收藏点
    private int commonName;

    public SceneMapPointSearchView(@NonNull Context context) {
        super(context);
    }

    public SceneMapPointSearchView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneMapPointSearchView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneMapPointSearchViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneMapPointSearchViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneMapPointSearchViewImpl initSceneImpl() {
        return new SceneMapPointSearchViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setSceneMapPointSearchView(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        isMapPointSearchFragmentShow = true;
        intSearchLoadingDialog();
    }

    private void intSearchLoadingDialog() {
        searchLoadingDialog = new SearchLoadingDialog(getContext());
    }

    public void onSearchResult(SearchResultEntity searchResultEntity) {
        if (null != searchLoadingDialog) {
            searchLoadingDialog.hide();
        }
        if (null == searchResultEntity || searchResultEntity.getPoiList().isEmpty()) {
            ToastUtils.Companion.getInstance().showCustomToastView("暂无数据");
            return;
        }
        this.poiInfoEntity = searchResultEntity.getPoiList().get(0);
        if (mViewBinding != null) {
            mViewBinding.skPoiName.setText(searchResultEntity.getPoiList().get(0).getName());
            mViewBinding.poiSecondAddress.setText(searchResultEntity.getPoiList().get(0).getAddress());
//            mViewBinding.poiBusinessHours.setText("营业时间 :" + searchResultEntity.getPoiList().get(0).getBusinessTime());
//            mViewBinding.poiPhone.setText("电       话 :" + searchResultEntity.getPoiList().get(0).getPhone());
        } else {
            Logger.d(SEARCH_HMI_TAG, "mViewBinding is null");
        }
    }

    public void doSearch(PoiInfoEntity poiInfo) {
        if (null != searchLoadingDialog) {
            searchLoadingDialog.show();
        }
        mScreenViewModel.doSearch(poiInfo);
    }

    /**
     * 刷新poi视图
     *
     * @param poiType poi类型
     *                1:家 2:公司 3:常用地址 0:收藏夹
     */
    public void refreshPoiView(int poiType) {
        //根据入口场景刷新poi视图,// 1:家 2:公司 3:常用地址 0:收藏夹
        Logger.d(SEARCH_HMI_TAG, "poiType: " + poiType);
        SceneMapPointSearchView.poiType = poiType;
        switch (poiType) {
            case AutoMapConstant.HomeCompanyType.HOME:
                mViewBinding.stvSetting.setText(R.string.mps_set_home);
                hintText = getContext().getString(R.string.mps_set_home_success);
                commonName = HomeCompanyType.HOME;
                break;
            case HomeCompanyType.COMPANY:
                mViewBinding.stvSetting.setText(R.string.mps_set_company);
                hintText = getContext().getString(R.string.mps_set_company_success);
                commonName = HomeCompanyType.COMPANY;
                break;
            case HomeCompanyType.COLLECTION:
                mViewBinding.stvSetting.setText(R.string.mps_set_add);
                hintText = getContext().getString(R.string.smp_set_success);
                commonName = HomeCompanyType.COLLECTION;
                break;
            case HomeCompanyType.COMMON:
                mViewBinding.stvSetting.setText(R.string.mps_set_add);
                hintText = getContext().getString(R.string.smp_set_success);
                commonName = HomeCompanyType.COMMON;
                break;
            default:
                break;

        }
    }

    public void clickSetting() {
        ToastUtils.Companion.getInstance().showCustomToastView(hintText);
        //点击添加设置家、公司、常用地址、收藏等commonName (1家，2公司,3常用地址，0普通收藏点）
        FavoriteInfo favoriteInfo = new FavoriteInfo();
        favoriteInfo.setCommonName(commonName)
                .setItemId(poiInfoEntity.getPid() + "_" + poiInfoEntity.getName() + "_" + poiInfoEntity.getPoint().getLon() + "_" + poiInfoEntity.getPoint().getLat())
                .setUpdateTime(new Date().getTime());
        poiInfoEntity.setFavoriteInfo(favoriteInfo);
        BehaviorPackage.getInstance().addFavoriteData(poiInfoEntity, commonName);
        SettingUpdateObservable.getInstance().onUpdateSyncTime();
        closeAllFragment();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        SceneMapPointSearchView.isMapPointSearchFragmentShow = false;
    }
}
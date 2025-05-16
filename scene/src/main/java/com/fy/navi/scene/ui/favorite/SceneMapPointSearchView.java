package com.fy.navi.scene.ui.favorite;


import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.api.search.ISceneTerminalParking;
import com.fy.navi.scene.databinding.SceneMapPointSearchViewBinding;
import com.fy.navi.scene.impl.favorite.SceneMapPointSearchViewImpl;
import com.fy.navi.scene.ui.search.SearchLoadingDialog;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.AutoMapConstant.HomeCompanyType;
import com.fy.navi.service.AutoMapConstant.PoiType;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;

import java.util.Date;

/**
 * @author qlzou
 * @version \$Revision1.0\$
 * @Description: 地图选点
 * @CreateDate: $ $
 */
public class SceneMapPointSearchView extends BaseSceneView<SceneMapPointSearchViewBinding, SceneMapPointSearchViewImpl> {
    private static final String DIVIDER = "_";
    @PoiType
    private int mPoiType;
    private String mHintText = "";
    private SearchLoadingDialog mSearchLoadingDialog;
    private PoiInfoEntity mPoiInfoEntity;
    //common_name：1，家  2，公司 3.常用地址  0，普通收藏点
    private int mCommonName;
    private ISceneTerminalParking mClickListener;
    public void setClickListener(final ISceneTerminalParking clickListener) {
        this.mClickListener = clickListener;
    }

    public SceneMapPointSearchView(@NonNull final Context context) {
        super(context);
    }

    public SceneMapPointSearchView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneMapPointSearchView(@NonNull final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneMapPointSearchViewBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
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
        intSearchLoadingDialog();
    }

    /**
     * 初始化加载弹窗
     */
    private void intSearchLoadingDialog() {
        mSearchLoadingDialog = new SearchLoadingDialog(getContext());
    }

    /**
     * 搜索结果回调
     * @param searchResultEntity searchResultEntity
     */
    public void onSearchResult(final SearchResultEntity searchResultEntity) {
        if (null != mSearchLoadingDialog) {
            mSearchLoadingDialog.hide();
        }
        ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
        if (null == searchResultEntity || searchResultEntity.getPoiList().isEmpty()) {
            ToastUtils.Companion.getInstance().showCustomToastView("暂无数据");
            ThreadManager.getInstance().postUi(mTimeoutTask);
            return;
        }
        this.mPoiInfoEntity = searchResultEntity.getPoiList().get(0);
        if (mViewBinding != null) {
            mViewBinding.skPoiName.setText(searchResultEntity.getPoiList().get(0).getName());
            mViewBinding.poiSecondAddress.setText(searchResultEntity.getPoiList().get(0).getAddress());
//            mViewBinding.poiBusinessHours.setText("营业时间 :" + searchResultEntity.getPoiList().get(0).getBusinessTime());
//            mViewBinding.poiPhone.setText("电       话 :" + searchResultEntity.getPoiList().get(0).getPhone());
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "mViewBinding is null");
        }
    }

    /**
     * 执行搜索方法
     * @param poiInfo poiInfo
     */
    public void doSearch(final PoiInfoEntity poiInfo) {
        if (null != mSearchLoadingDialog && mSearchLoadingDialog.isShowing()) {
            Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "mSearchLoadingDialog is showing");
        } else {
            mSearchLoadingDialog = new SearchLoadingDialog(getContext());
            mSearchLoadingDialog.show();
        }
        mPoiInfoEntity = poiInfo;
        mScreenViewModel.doSearch(poiInfo);
        ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
        ThreadManager.getInstance().postDelay(mTimeoutTask, 6000);
    }

    private final Runnable mTimeoutTask = new Runnable() {
        @Override
        public void run() {
            if (null != mSearchLoadingDialog) {
                mSearchLoadingDialog.dismiss();
                if (!ConvertUtils.isEmpty(mViewBinding) && !ConvertUtils.isEmpty(mViewBinding.csPoiNoResult)) {
                    mViewBinding.csPoiNoResult.setVisibility(View.VISIBLE);
                    mViewBinding.poiTypeIcon.setVisibility(View.VISIBLE);
                    mViewBinding.skPoiName.setVisibility(View.GONE);
                    mViewBinding.poiSecondAddress.setVisibility(View.GONE);
                    mViewBinding.stlSetting.setVisibility(View.GONE);

                    mViewBinding.noResultButton.setOnClickListener((view) -> {
                        doSearch(mPoiInfoEntity);
                        mViewBinding.csPoiNoResult.setVisibility(View.GONE);
                        mViewBinding.skPoiName.setVisibility(View.VISIBLE);
                        mViewBinding.poiSecondAddress.setVisibility(View.VISIBLE);
                        mViewBinding.stlSetting.setVisibility(View.VISIBLE);
                        mViewBinding.poiTypeIcon.setVisibility(View.VISIBLE);
                    });
                }
            }
        }
    };

    /**
     * 刷新poi视图
     *
     * @param poiType poi类型
     *                1:家 2:公司 3:常用地址 0:收藏夹
     */
    public void refreshPoiView(final int poiType) {
        //根据入口场景刷新poi视图,// 1:家 2:公司 3:常用地址 0:收藏夹
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poiType: " + poiType);
        this.mPoiType = poiType;
        switch (poiType) {
            case AutoMapConstant.HomeCompanyType.HOME:
                mViewBinding.stvSetting.setText(R.string.mps_set_home);
                mHintText = getContext().getString(R.string.mps_set_home_success);
                mCommonName = HomeCompanyType.HOME;
                break;
            case HomeCompanyType.COMPANY:
                mViewBinding.stvSetting.setText(R.string.mps_set_company);
                mHintText = getContext().getString(R.string.mps_set_company_success);
                mCommonName = HomeCompanyType.COMPANY;
                break;
            case HomeCompanyType.COLLECTION:
                mViewBinding.stvSetting.setText(R.string.mps_set_add);
                mHintText = getContext().getString(R.string.smp_set_add);
                mCommonName = HomeCompanyType.COLLECTION;
                break;
            case HomeCompanyType.COMMON:
                mViewBinding.stvSetting.setText(R.string.mps_set_add);
                mHintText = getContext().getString(R.string.smp_set_add);
                mCommonName = HomeCompanyType.COMMON;
                break;
            case HomeCompanyType.ALONG_WAY:
                mViewBinding.stvSetting.setText(R.string.route_service_details_add_via);
                mHintText = null;
                mCommonName = HomeCompanyType.ALONG_WAY;
                break;
            default:
                mCommonName = HomeCompanyType.HOME;
                break;

        }
        mScreenViewModel.setCommonName(mCommonName);
    }

    /**
     * 关闭地图选点页面
     */
    public void closeMapPointView() {
        if (null != mClickListener) {
            mClickListener.closeSearch();
        }
    }

    /**
     * 注册点击事件
     */
    public void clickSetting() {
        if (mHintText != null) {
            ToastUtils.Companion.getInstance().showCustomToastView(mHintText);
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "clickSetting: " + mCommonName
                + " mPoiInfoEntity: " + mPoiInfoEntity);
        if (ConvertUtils.isEmpty(mPoiInfoEntity)) {
            return;
        }
        if (mCommonName == HomeCompanyType.ALONG_WAY) {
            if (SearchPackage.getInstance().isAlongWaySearch()) {
                RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, mPoiInfoEntity);
                Logger.d("mapMsgPushInfoToPoiInfoEntity1: " + GsonUtils.toJson(mPoiInfoEntity));
            }
            closeAllFragmentsUntilTargetFragment("MainAlongWaySearchFragment");
            showCurrentFragment();
        } else {
            //点击添加设置家、公司、常用地址、收藏等commonName (1家，2公司,3常用地址，0普通收藏点）
            final FavoriteInfo favoriteInfo = new FavoriteInfo();
            favoriteInfo.setCommonName(mCommonName)
                    .setUpdateTime(new Date().getTime());
            mPoiInfoEntity.setFavoriteInfo(favoriteInfo);
            if (ConvertUtils.isEmpty(mPoiInfoEntity.getPid())) {
                //逆地理搜索出的点无poiId，需自己拼接
                mPoiInfoEntity.setPid(mPoiInfoEntity.getPoint().getLon() + ""
                        + mPoiInfoEntity.getPoint().getLat());
            }
            BehaviorPackage.getInstance().addFavorite(mPoiInfoEntity, mCommonName);
//            BehaviorPackage.getInstance().addFavoriteData(mPoiInfoEntity, mCommonName);
            SettingUpdateObservable.getInstance().onUpdateSyncTime();
//            if (mCommonName == HomeCompanyType.COLLECTION) {
            closeAllFragmentsUntilTargetFragment("HomeCompanyFragment");
            showCurrentFragment();
//            } else {
//                closeAllFragment();
//            }
        }
        mScreenViewModel.clearPoiLabelMark();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
    }
}
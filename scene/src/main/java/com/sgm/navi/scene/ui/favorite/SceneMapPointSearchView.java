package com.sgm.navi.scene.ui.favorite;


import android.animation.ValueAnimator;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.LinearInterpolator;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneView;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.api.search.ISceneTerminalParking;
import com.sgm.navi.scene.databinding.SceneMapPointSearchViewBinding;
import com.sgm.navi.scene.impl.favorite.SceneMapPointSearchViewImpl;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.AutoMapConstant.HomeCompanyType;
import com.sgm.navi.service.AutoMapConstant.PoiType;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.FavoriteInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.sgm.navi.service.logicpaket.user.behavior.BehaviorPackage;

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
    private PoiInfoEntity mPoiInfoEntity;
    //common_name：1，家  2，公司 3.常用地址  0，普通收藏点
    private int mCommonName;
    private ISceneTerminalParking mClickListener;
    private ValueAnimator mAnimator;
    private float mAngelTemp = 0;
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
        initLoadAnim(mViewBinding.ivLoading);
    }

    /**
     * 初始化加载弹窗
     */
    private void intSearchLoadingDialog() {
    }

    /**
     * 初始化加载动画
     * @param sivLoading 加载动画视图
     */
    private void initLoadAnim(final View sivLoading) {
        // 如果动画已存在并正在运行，则取消并清理
        if (mAnimator != null) {
            if (mAnimator.isRunning()) {
                mAnimator.cancel();
            }
            mAnimator = null;
        }

        // 创建属性动画，从 0 到 360 度循环旋转
        mAnimator = ValueAnimator.ofFloat(0f, 360f);
        mAnimator.setDuration(2000); // 动画持续时间
        mAnimator.setRepeatCount(ValueAnimator.INFINITE); // 无限重复
        mAnimator.setInterpolator(new LinearInterpolator()); // 线性插值器
        // 添加动画更新监听器
        mAnimator.addUpdateListener(animation -> {
            final float angle = (float) animation.getAnimatedValue();
            if (shouldSkipUpdate(angle)) {
                return;
            }
            sivLoading.setRotation(angle);
        });
    }

    /**
     *用于控制角度变化频率的辅助方法
     *@param angle 当前角度
     *@return 是否跳过更新
     */
    private boolean shouldSkipUpdate(final float angle) {
        final float changeAngle = angle - mAngelTemp;
        final float angleStep = 10;
        if (changeAngle > 0f && changeAngle <= angleStep) {
            return true; // 跳过更新，避免高频调用浪费资源
        }
        mAngelTemp = angle; // 更新临时角度值
        return false;
    }

    /**
     * 搜索结果回调
     * @param taskId 任务id
     * @param searchResultEntity searchResultEntity
     */
    public void onSearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
        if (null == searchResultEntity || searchResultEntity.getPoiList().isEmpty()) {
            ToastUtils.Companion.getInstance().showCustomToastView("暂无数据");
            ThreadManager.getInstance().postUi(mTimeoutTask);
            return;
        }
        if (ConvertUtils.isEmpty(mScreenViewModel)) {
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "taskId: " + taskId
                + " currentId: " + mScreenViewModel.getMTaskId());
        if (!ConvertUtils.equals(taskId, mScreenViewModel.getMTaskId()) && mScreenViewModel.getMTaskId() != 0) {
            return;
        }
        showLoading(false);
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
        Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "doSearch ");
        mPoiInfoEntity = poiInfo;
        mScreenViewModel.doSearch(poiInfo);
        showLoading(true);
        ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
        ThreadManager.getInstance().postDelay(mTimeoutTask, 6000);
    }

    private final Runnable mTimeoutTask = new Runnable() {
        @Override
        public void run() {
            if (!ConvertUtils.isEmpty(mViewBinding) && !ConvertUtils.isEmpty(mViewBinding.csPoiNoResult)) {
                mViewBinding.csPoiNoResult.setVisibility(View.VISIBLE);
                mViewBinding.noResultButton.setVisibility(View.VISIBLE);
                mViewBinding.noResultHint.setText(getContext().getString(R.string.load_failed));
                mViewBinding.skPoiName.setVisibility(View.GONE);
                mViewBinding.poiSecondAddress.setVisibility(View.GONE);
                mViewBinding.stlSetting.setVisibility(View.GONE);
                mViewBinding.ivLoading.setVisibility(View.GONE);
                if (mAnimator != null) {
                    mAnimator.cancel();
                }
//                mScreenViewModel.flyLineVisible(true);
                mViewBinding.noResultButton.setOnClickListener((view) -> {
                    doSearch(mPoiInfoEntity);
                    if (mAnimator != null) {
                        mAnimator.start();
                    }
                });
            }
        }
    };

    /**
     * 展示加载动画
     * @param isShow 是否需要显示
     */
    private void showLoading(final boolean isShow) {
//        mScreenViewModel.flyLineVisible(!isShow);
        mViewBinding.csPoiNoResult.setVisibility(isShow ? View.VISIBLE : View.GONE);
        mViewBinding.noResultHint.setText(getContext().getString(R.string.address_loading));
        mViewBinding.ivLoading.setVisibility(isShow ? View.VISIBLE : View.GONE);
        mViewBinding.noResultButton.setVisibility(View.GONE);
        if (mAnimator != null) {
            if (isShow) {
                mAnimator.start();
            } else {
                mAnimator.cancel();
            }
        }
        mViewBinding.skPoiName.setVisibility(isShow ? View.GONE : View.VISIBLE);
        mViewBinding.poiSecondAddress.setVisibility(isShow ? View.GONE : View.VISIBLE);
        mViewBinding.stlSetting.setVisibility(isShow ? View.GONE : View.VISIBLE);
    }

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
        isFavoriteOrViaPoint();
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
     * 判断数据是否已收藏或是否是途经点
     */
    private void isFavoriteOrViaPoint() {
        boolean mIsFavoriteOrViaPoint = false;
        if (mPoiType == AutoMapConstant.HomeCompanyType.ALONG_WAY) {
            mIsFavoriteOrViaPoint = RoutePackage.getInstance().isBelongRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, mPoiInfoEntity);
        } else if(mPoiType == HomeCompanyType.COLLECTION){
            mIsFavoriteOrViaPoint = !BehaviorPackage.getInstance().isHomeOrCompanyOrFavorite(mPoiInfoEntity).isEmpty();
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"IsFavoriteOrViaPoint: " + mIsFavoriteOrViaPoint);
        if (mIsFavoriteOrViaPoint) {
            mViewBinding.stvSetting.setEnabled(false);
            mViewBinding.stvSetting.setAlpha(0.5f);
        }
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
                if (Logger.openLog) {
                    Logger.d("mapMsgPushInfoToPoiInfoEntity1: ", mPoiInfoEntity);
                }
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
                mPoiInfoEntity.setPid(mPoiInfoEntity.getPoint().getLon() + "_"
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
        mAnimator.cancel();
        ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
    }

    public void onBackPressed(){
     if(null != mScreenViewModel) mScreenViewModel.closeFragment();
    }
}
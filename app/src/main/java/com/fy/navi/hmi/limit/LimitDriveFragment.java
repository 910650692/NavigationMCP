package com.fy.navi.hmi.limit;

import android.os.Bundle;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.databinding.library.baseAdapters.BR;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentLimitDetailBinding;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.aos.RestrictedArea;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.route.RouteRestrictionParam;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.ui.base.BaseFragment;

import java.util.ArrayList;

/**
 * @author QiuYaWei
 * @version  \$Revision.1.0\$
 * Date: 2025/2/7
 * Description: [限行详情页面]
 */
public class LimitDriveFragment extends BaseFragment<FragmentLimitDetailBinding, LimitDriverViewModel> {
    private LimitDriverAdapter mAdapter;
    private LimitDriverCitiesAdapter mCitiesAdapter;
    private LinearLayoutManager mLayoutManager;
    private static final String TAG = "LimitProvincesAdapter";

    @Override
    public int onLayoutId() {
        return R.layout.fragment_limit_detail;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        mAdapter = new LimitDriverAdapter(requireContext(), new ArrayList<>());
        mBinding.recyclerView.setLayoutManager(new LinearLayoutManager(requireContext()));
        mBinding.recyclerView.setAdapter(mAdapter);
        mCitiesAdapter = new LimitDriverCitiesAdapter(requireContext(), new ArrayList<>());
        mLayoutManager = new LinearLayoutManager(requireContext(), LinearLayoutManager.HORIZONTAL, false);
        mBinding.citiesRecyclerView.setLayoutManager(mLayoutManager);
        mBinding.citiesRecyclerView.setAdapter(mCitiesAdapter);
    }

    @Override
    public void onInitData() {
        onInitClick();
        final Bundle bundle = getArguments();
        if (bundle == null) {
            return;
        }

        //城市选择界面跳转
        final String cityCode = (String) bundle.getSerializable(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_LIMIT_CITY_TASK_ID);
        if (cityCode != null) {
            showLoading();
            mViewModel.queryLimitPolicyByCityCode(cityCode);
            return;
        }

        //算路界面跳转
        final RouteRestrictionParam roundParam = (RouteRestrictionParam) bundle
                .getSerializable(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_LIMIT_ROUND);
        if (roundParam != null) {
            LimitDriverHelper.getInstance().setNeedClearRestriction(false);
            LimitDriverHelper.getInstance().setRoundParam(roundParam);
            showPolicyUI(roundParam);
            return;
        }

        //主图按钮跳转
        final RouteRestrictionParam routeRestrictionParam = (RouteRestrictionParam) bundle
                .getSerializable(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_LIMIT_DRIVER);
        if (routeRestrictionParam != null) {
            LimitDriverHelper.getInstance().setNeedClearRestriction(true);
            LimitDriverHelper.getInstance().setRoundParam(null);
            RoutePackage.getInstance().drawRestrictionForLimit(MapTypeId.MAIN_SCREEN_MAIN_MAP,
                    routeRestrictionParam.getMReStrictedAreaResponseParam(), 0);
            showPolicyUI(routeRestrictionParam);
        }
    }

    /**
     * 点击事件初始化
     */
    public void onInitClick() {
        mBinding.tvRetry.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(final View v) {
                showLoading();
                mViewModel.queryRetry();
            }
        });

        mBinding.ivItemPre.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(final View v) {
                scrollToPrevious();
            }
        });

        mBinding.ivItemNext.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(final View v) {
                scrollToNext();
            }
        });

        mBinding.citiesRecyclerView.addOnScrollListener(new RecyclerView.OnScrollListener() {
            @Override
            public void onScrolled(final @NonNull RecyclerView recyclerView, final int dx, final int dy) {
                super.onScrolled(recyclerView, dx, dy);
                updateButtonStates();
            }
        });

        updateButtonStates();
    }

    /**
     * 点击上一页
     */
    private void scrollToPrevious() {
        final int firstVisibleItemPosition = mLayoutManager.findFirstVisibleItemPosition();
        if (firstVisibleItemPosition > 0) {
            mLayoutManager.scrollToPositionWithOffset(firstVisibleItemPosition - 1, 0);
        } else {
            mBinding.ivItemPre.setEnabled(false);
        }
    }

    /**
     * 点击下一页
     */
    private void scrollToNext() {
        final int lastVisibleItemPosition = mLayoutManager.findLastVisibleItemPosition();
        final int totalItemCount = mLayoutManager.getItemCount();
        if (lastVisibleItemPosition < totalItemCount - 1) {
            mLayoutManager.scrollToPositionWithOffset(lastVisibleItemPosition + 1, 0);
        } else {
            mBinding.ivItemNext.setEnabled(false);
        }
    }

    /**
     * 改变按钮图标
     */
    private void updateButtonStates() {
        final int firstVisibleItemPosition = mLayoutManager.findFirstVisibleItemPosition();
        final int lastVisibleItemPosition = mLayoutManager.findLastVisibleItemPosition();
        final int totalItemCount = mLayoutManager.getItemCount();

        mBinding.ivItemPre.setEnabled(firstVisibleItemPosition > 0);
        mBinding.ivItemNext.setEnabled(lastVisibleItemPosition < totalItemCount - 1);
    }

    /**
     * 显示加载中界面
     */
    public void showLoading() {
        ThreadManager.getInstance().postUi(() -> {
            mBinding.layoutLoading.setVisibility(View.VISIBLE);
            mBinding.layoutPolicy.setVisibility(View.GONE);
            mBinding.tvLoading.setText(R.string.limit_loading);
            mBinding.tvRetry.setVisibility(View.GONE);
        });
    }

    /**
     * 显示加载失败界面
     */
    public void showLoadingFail() {
        ThreadManager.getInstance().postUi(() -> {
            mBinding.layoutLoading.setVisibility(View.VISIBLE);
            mBinding.layoutPolicy.setVisibility(View.GONE);
            mBinding.tvLoading.setText(R.string.limit_load_fail);
            mBinding.tvRetry.setVisibility(View.VISIBLE);
        });
    }

    /**
     * 显示限行政策界面
     * @param routeRestrictionParam 政策信息
     */
    public void showPolicyUI(final RouteRestrictionParam routeRestrictionParam) {
        final RestrictedArea restrictedArea = routeRestrictionParam.getMRestrictedArea();
        ThreadManager.getInstance().postUi(() -> {
            if (restrictedArea != null) {
                mViewModel.setSelectedCityName(restrictedArea.getMCityNames().get(0));
                mBinding.layoutLoading.setVisibility(View.GONE);
                mBinding.layoutPolicy.setVisibility(View.VISIBLE);
                if (restrictedArea.getMCityNames().size() > 1) {
                    mBinding.tvCity.setVisibility(View.GONE);
                    mBinding.layoutMultiCity.setVisibility(View.VISIBLE);
                    mCitiesAdapter.setData(restrictedArea.getMCityNames());
                    mCitiesAdapter.setListener(new LimitDriverCitiesAdapter.ItemClickListener() {
                        @Override
                        public void onClick(final int position) {
                            if (!restrictedArea.getMCityNames().get(position).isEmpty()) {
                                mViewModel.setSelectedCityName(restrictedArea.getMCityNames().get(position));
                                mBinding.recyclerView.setVisibility(View.VISIBLE);
                                mAdapter.setData(restrictedArea.getMRestrictedAreaDetails().get(position));
                                mBinding.tvNoContent.setVisibility(View.GONE);
                                //绘制限行区域，地图中心跳转
                                RoutePackage.getInstance().drawRestrictionForLimit(MapTypeId.MAIN_SCREEN_MAIN_MAP,
                                        routeRestrictionParam.getMReStrictedAreaResponseParam(), restrictedArea.getMCityPosition().get(position));
                                final int cityCode = MapDataPackage.getInstance().searchCityAdCode(restrictedArea.getMCityNames().get(position));
                                if (cityCode != 0) {
                                    final CityDataInfo cityItemBean= MapDataPackage.getInstance().getCityInfo(cityCode);
                                    MapPackage.getInstance().setMapCenter(MapTypeId.MAIN_SCREEN_MAIN_MAP,
                                            new GeoPoint(ConvertUtils.transCityLatAndLon(cityItemBean.getCityX()),
                                                    ConvertUtils.transCityLatAndLon(cityItemBean.getCityY())));
                                }
                            } else {
                                mBinding.recyclerView.setVisibility(View.GONE);
                                mBinding.tvNoContent.setVisibility(View.VISIBLE);
                            }
                        }
                    });
                } else {
                    mBinding.tvCity.setVisibility(View.VISIBLE);
                    mBinding.layoutMultiCity.setVisibility(View.GONE);
                    mBinding.tvCity.setText(restrictedArea.getMCityNames().get(0));
                }

                if (!restrictedArea.getMRestrictedAreaDetails().isEmpty()) {
                    mBinding.recyclerView.setVisibility(View.VISIBLE);
                    mAdapter.setData(restrictedArea.getMRestrictedAreaDetails().get(0));
                    mBinding.tvNoContent.setVisibility(View.GONE);
                } else {
                    mBinding.recyclerView.setVisibility(View.GONE);
                    mBinding.tvNoContent.setVisibility(View.VISIBLE);
                }
            }
        });
    }
}

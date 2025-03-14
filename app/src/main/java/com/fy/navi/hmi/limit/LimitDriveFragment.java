package com.fy.navi.hmi.limit;

import static android.view.View.GONE;
import static android.view.View.VISIBLE;

import static com.fy.navi.service.define.map.MapTypeId.MAIN_SCREEN_MAIN_MAP;

import android.os.Bundle;
import android.view.View;

import androidx.databinding.library.baseAdapters.BR;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

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
 * Author: QiuYaWei
 * Date: 2025/2/7
 * Description: [限行详情页面]
 */
public class LimitDriveFragment extends BaseFragment<FragmentLimitDetailBinding, LimitDriverViewModel> {
    private LimitDriverAdapter adapter;
    private LimitDriverCitiesAdapter citiesAdapter;
    private LinearLayoutManager layoutManager;
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
        adapter = new LimitDriverAdapter(requireContext(), new ArrayList<>());
        mBinding.recyclerView.setLayoutManager(new LinearLayoutManager(requireContext()));
        mBinding.recyclerView.setAdapter(adapter);
        citiesAdapter = new LimitDriverCitiesAdapter(requireContext(), new ArrayList<>());
        layoutManager = new LinearLayoutManager(requireContext(), LinearLayoutManager.HORIZONTAL, false);
        mBinding.citiesRecyclerView.setLayoutManager(layoutManager);
        mBinding.citiesRecyclerView.setAdapter(citiesAdapter);
    }

    @Override
    public void onInitData() {
        onInitClick();
        Bundle bundle = getArguments();
        //City selection
        String cityCode = (String) bundle.getSerializable(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_LIMIT_CITY_TASK_ID);
        if (cityCode != null) {
            showLoading();
            mViewModel.queryLimitPolicyByCityCode(cityCode);
            return;
        }

        RouteRestrictionParam roundParam = (RouteRestrictionParam) bundle.getSerializable(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_LIMIT_ROUND);
        if (roundParam != null) {
            LimitDriverHelper.getInstance().setNeedClearRestriction(false);
            LimitDriverHelper.getInstance().setRoundParam(roundParam);
            showPolicyUI(roundParam);
            return;
        }

        RouteRestrictionParam routeRestrictionParam = (RouteRestrictionParam) bundle.getSerializable(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_LIMIT_DRIVER);
        if (routeRestrictionParam != null) {
            LimitDriverHelper.getInstance().setNeedClearRestriction(true);
            LimitDriverHelper.getInstance().setRoundParam(null);
            RoutePackage.getInstance().drawRestrictionForLimit(MapTypeId.MAIN_SCREEN_MAIN_MAP,
                    routeRestrictionParam.getGReStrictedAreaResponseParam(), 0);
            showPolicyUI(routeRestrictionParam);
        }
    }

    public void onInitClick() {
        mBinding.tvRetry.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                showLoading();
                mViewModel.queryRetry();
            }
        });

        mBinding.ivItemPre.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                scrollToPrevious();
            }
        });

        mBinding.ivItemNext.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                scrollToNext();
            }
        });

        mBinding.citiesRecyclerView.addOnScrollListener(new RecyclerView.OnScrollListener() {
            @Override
            public void onScrolled(RecyclerView recyclerView, int dx, int dy) {
                super.onScrolled(recyclerView, dx, dy);
                updateButtonStates();
            }
        });

        updateButtonStates();
    }

    public double TransCityLatAndLon(double input) {
        double scaleFactor = 1000000.0;
        return input / scaleFactor;
    }

    private void scrollToPrevious() {
        int firstVisibleItemPosition = layoutManager.findFirstVisibleItemPosition();
        if (firstVisibleItemPosition > 0) {
            layoutManager.scrollToPositionWithOffset(firstVisibleItemPosition - 1, 0);
        } else {
            mBinding.ivItemPre.setEnabled(false);
        }
    }

    private void scrollToNext() {
        int lastVisibleItemPosition = layoutManager.findLastVisibleItemPosition();
        int totalItemCount = layoutManager.getItemCount();
        if (lastVisibleItemPosition < totalItemCount - 1) {
            layoutManager.scrollToPositionWithOffset(lastVisibleItemPosition + 1, 0);
        } else {
            mBinding.ivItemNext.setEnabled(false);
        }
    }

    private void updateButtonStates() {
        int firstVisibleItemPosition = layoutManager.findFirstVisibleItemPosition();
        int lastVisibleItemPosition = layoutManager.findLastVisibleItemPosition();
        int totalItemCount = layoutManager.getItemCount();

        mBinding.ivItemPre.setEnabled(firstVisibleItemPosition > 0);
        mBinding.ivItemNext.setEnabled(lastVisibleItemPosition < totalItemCount - 1);
    }

    public void showLoading() {
        ThreadManager.getInstance().postUi(() -> {
            mBinding.layoutLoading.setVisibility(VISIBLE);
            mBinding.layoutPolicy.setVisibility(GONE);
            mBinding.tvLoading.setText(R.string.limit_loading);
            mBinding.tvRetry.setVisibility(GONE);
        });
    }

    public void showLoadingFail() {
        ThreadManager.getInstance().postUi(() -> {
            mBinding.layoutLoading.setVisibility(VISIBLE);
            mBinding.layoutPolicy.setVisibility(GONE);
            mBinding.tvLoading.setText(R.string.limit_load_fail);
            mBinding.tvRetry.setVisibility(VISIBLE);
        });
    }

    public void showPolicyUI(RouteRestrictionParam routeRestrictionParam) {
        RestrictedArea restrictedArea = routeRestrictionParam.getRestrictedArea();
        ThreadManager.getInstance().postUi(() -> {
            if (restrictedArea != null) {
                mViewModel.setSelectedCityName(restrictedArea.cityNames.get(0));
                mBinding.layoutLoading.setVisibility(GONE);
                mBinding.layoutPolicy.setVisibility(VISIBLE);
                if (restrictedArea.getCityNames().size() > 1) {
                    mBinding.tvCity.setVisibility(GONE);
                    mBinding.layoutMultiCity.setVisibility(VISIBLE);
                    citiesAdapter.setData(restrictedArea.getCityNames());
                    citiesAdapter.setListener(new LimitDriverCitiesAdapter.ItemClickListener() {
                        @Override
                        public void onClick(int position) {
                            if (!restrictedArea.cityNames.get(position).isEmpty()) {
                                mViewModel.setSelectedCityName(restrictedArea.cityNames.get(position));
                                mBinding.recyclerView.setVisibility(VISIBLE);
                                adapter.setData(restrictedArea.restrictedAreaDetails.get(position));
                                mBinding.tvNoContent.setVisibility(GONE);
                                //绘制限行区域，地图中心跳转
                                RoutePackage.getInstance().drawRestrictionForLimit(MapTypeId.MAIN_SCREEN_MAIN_MAP,
                                        routeRestrictionParam.getGReStrictedAreaResponseParam(), restrictedArea.cityPosition.get(position));
                                int cityCode = MapDataPackage.getInstance().searchCityAdCode(restrictedArea.cityNames.get(position));
                                if (cityCode != 0) {
                                    CityDataInfo cityItemBean= MapDataPackage.getInstance().getCityInfo(cityCode);
                                    MapPackage.getInstance().setMapCenter(MAIN_SCREEN_MAIN_MAP,
                                            new GeoPoint(TransCityLatAndLon(cityItemBean.cityX), TransCityLatAndLon(cityItemBean.cityY)));
                                }
                            } else {
                                mBinding.recyclerView.setVisibility(GONE);
                                mBinding.tvNoContent.setVisibility(VISIBLE);
                            }
                        }
                    });
                } else {
                    mBinding.tvCity.setVisibility(VISIBLE);
                    mBinding.layoutMultiCity.setVisibility(GONE);
                    mBinding.tvCity.setText(restrictedArea.cityNames.get(0));
                }

                if (!restrictedArea.restrictedAreaDetails.isEmpty()) {
                    mBinding.recyclerView.setVisibility(VISIBLE);
                    adapter.setData(restrictedArea.restrictedAreaDetails.get(0));
                    mBinding.tvNoContent.setVisibility(GONE);
                } else {
                    mBinding.recyclerView.setVisibility(GONE);
                    mBinding.tvNoContent.setVisibility(VISIBLE);
                }
            }
        });
    }
}

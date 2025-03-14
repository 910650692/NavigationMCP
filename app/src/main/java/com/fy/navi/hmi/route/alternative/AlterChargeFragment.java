package com.fy.navi.hmi.route.alternative;

import android.os.Bundle;
import android.view.View;

import androidx.recyclerview.widget.LinearLayoutManager;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentAlterChargeBinding;
import com.fy.navi.hmi.route.AlterChargeViewModel;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.route.RouteAlterChargeStationInfo;
import com.fy.navi.service.define.route.RouteAlterChargeStationParam;
import com.fy.navi.service.define.route.RouteChargeStationDetailInfo;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.ServiceAreaInfo;
import com.fy.navi.ui.action.ViewAdapterKt;
import com.fy.navi.ui.base.BaseFragment;

import java.util.ArrayList;
import java.util.List;

/**
 * Author: LiuChang
 * Date: 2025/3/6
 * Description: [替换补充能点界面]
 */
@Route(path = RoutePath.Route.ALTER_CHARGE_FRAGMENT)
public class AlterChargeFragment extends BaseFragment<FragmentAlterChargeBinding, AlterChargeViewModel> {
    private static final String TAG = "AlterChargeFragment";
    private AlterChargeStationAdapter adapter;
    private PoiInfoEntity detailPoiInfoEntity;


    @Override
    public int onLayoutId() {
        return R.layout.fragment_alter_charge;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        adapter = new AlterChargeStationAdapter(requireContext(), new ArrayList<>());
        mBinding.recyclerView.setLayoutManager(new LinearLayoutManager(requireContext()));
        mBinding.recyclerView.setAdapter(adapter);
    }

    @Override
    public void onInitData() {
        Bundle bundle = getArguments();
        //City selection
        RouteChargeStationDetailInfo routeChargeStationDetailInfo = (RouteChargeStationDetailInfo)
                bundle.getSerializable(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_ALTER_CHARGE_STATION);
        showAlterChargeStation(routeChargeStationDetailInfo);
        onInitClick();
    }

    @Override
    protected void onNewIntent(Bundle bundle) {
        super.onNewIntent(bundle);
        RouteChargeStationDetailInfo routeChargeStationDetailInfo = (RouteChargeStationDetailInfo)
                bundle.getSerializable(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_ALTER_CHARGE_STATION);
        showAlterChargeStation(routeChargeStationDetailInfo);

    }

    private void showAlterChargeStation(RouteChargeStationDetailInfo routeChargeStationDetailInfo) {
        if (routeChargeStationDetailInfo == null) {
            Logger.d(TAG, "routeChargeStationDetailInfo is null");
            return;
        }
        mViewModel.showAlterCharge.set(true);
        mBinding.tvSelectedChargeStation.setText(routeChargeStationDetailInfo.name);
        mViewModel.requestAlterChargeStation(routeChargeStationDetailInfo.poiID);
    }

    public void onInitClick() {
        adapter.setListener(new AlterChargeStationAdapter.ItemClickListener() {
            @Override
            public void onItemClick(String poiID) {
                mViewModel.getSearchDetailsMode(poiID);
            }

            @Override
            public void onAlterClick(RouteAlterChargeStationInfo info) {
                mViewModel.addViaList(info);
            }
        });

        mBinding.stlAlter.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (detailPoiInfoEntity != null) {
                    mViewModel.addViaList(detailPoiInfoEntity);
                }
            }
        });
    }

    public void showAlterChargeStationInfo(RouteAlterChargeStationParam routeAlterChargeStationParam) {
        if (routeAlterChargeStationParam == null) {
            Logger.d(TAG, "routeAlterChargeStationParam is null");
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            adapter.setData(routeAlterChargeStationParam.getRouteAlternativeChargeStationInfos());
        });

    }

    public void showChargeStationDetail(PoiInfoEntity poiInfoEntities) {
        if (poiInfoEntities == null) {
            Logger.d(TAG, "poiInfoEntities is null");
            return;
        }
        detailPoiInfoEntity = poiInfoEntities;
        ThreadManager.getInstance().postUi(() -> {
            mViewModel.showAlterCharge.set(false);
            mViewModel.routeSearchName.set(poiInfoEntities.getName());
            mViewModel.routeSearchAddress.set(poiInfoEntities.getAddress());
            ViewAdapterKt.loadImageUrl(mBinding.scenePoiDetailsChargingStationView.poiChargeImg, poiInfoEntities.getImageUrl(), com.fy.navi.scene.R.drawable.test_pic, com.fy.navi.scene.R.drawable.test_pic);
            if (!ConvertUtils.isEmpty(poiInfoEntities.getServiceAreaInfoList()) && !poiInfoEntities.getServiceAreaInfoList().isEmpty() && !ConvertUtils.isEmpty(poiInfoEntities.getServiceAreaInfoList().get(0))) {
                List<ServiceAreaInfo.ServiceAreaChild> serviceAreaChildList = poiInfoEntities.getServiceAreaInfoList().get(0).getServiceAreaChildList();
                int building = poiInfoEntities.getServiceAreaInfoList().get(0).getBuilding();
                mViewModel.routeSearchStatusVisibility.set(true);
                switch (building) {
                    case 0:
                        mViewModel.routeSearchStatusVisibility.set(false);
                        break;
                    case 1:
                        mViewModel.routeSearchStatus.set(ResourceUtils.Companion.getInstance().getString(R.string.route_details_building));
                        break;
                    case 2:
                        mViewModel.routeSearchStatus.set(ResourceUtils.Companion.getInstance().getString(R.string.route_details_not_find));
                        break;
                    case 3:
                        mViewModel.routeSearchStatus.set(ResourceUtils.Companion.getInstance().getString(R.string.route_details_starting));
                        break;
                    case 4:
                        mViewModel.routeSearchStatus.set(ResourceUtils.Companion.getInstance().getString(R.string.route_details_stoped));
                        break;
                }
            }
            if (!ConvertUtils.isEmpty(poiInfoEntities.getChargeInfoList()) && !poiInfoEntities.getChargeInfoList().isEmpty()) {
                ChargeInfo chargeInfo = poiInfoEntities.getChargeInfoList().get(0);
                mViewModel.routeSearchTypeVisibility.set(2);
                mBinding.scenePoiDetailsChargingStationView.poiChargeFastOccupied.setText(chargeInfo.getFast_free() + "");
                mBinding.scenePoiDetailsChargingStationView.poiChargeFastTotal.setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_jg) + chargeInfo.getFast_free());
                mBinding.scenePoiDetailsChargingStationView.poiChargeFastCurrentAndVlot.setText(chargeInfo.getFastPower() + "kw." + chargeInfo.getFastVolt() +"v");

                mBinding.scenePoiDetailsChargingStationView.poiChargeSlowOccupied.setText(chargeInfo.getSlow_free() + "");
                mBinding.scenePoiDetailsChargingStationView.poiChargeSlowTotal.setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_jg) + chargeInfo.getSlow_total());
                mBinding.scenePoiDetailsChargingStationView.poiChargeSlowCurrentAndVlot.setText(chargeInfo.getSlowPower() + "kw." + chargeInfo.getSlowVolt() +"v");

                mBinding.scenePoiDetailsChargingStationView.poiChargePrice.setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_charge_free) +chargeInfo.getCurrentElePrice() + ResourceUtils.Companion.getInstance().getString(R.string.route_details_charge_free_unit));
                mBinding.scenePoiDetailsChargingStationView.poiChargeParkPrice.setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_charge_park_free) +chargeInfo.getCurrentServicePrice());
            }

        });
    }
}

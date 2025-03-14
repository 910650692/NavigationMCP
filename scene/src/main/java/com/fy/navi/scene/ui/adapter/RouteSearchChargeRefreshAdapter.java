package com.fy.navi.scene.ui.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.RouteSearchChargeRefreshListItemBinding;
import com.fy.navi.scene.databinding.RouteSearchRefreshListItemBinding;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.service.logicpaket.route.RoutePackage;

import java.util.ArrayList;
import java.util.List;

public class RouteSearchChargeRefreshAdapter extends RecyclerView.Adapter<RouteSearchChargeRefreshAdapter.Holder> {
    private List<PoiInfoEntity> mRouteBeanList;
    private List<RouteParam> mLoaclSaveEntity;
    private int mSearchType;
    OnItemClickListener itemClickListener;
    public RouteSearchChargeRefreshAdapter() {
        mRouteBeanList = new ArrayList<>();
        mLoaclSaveEntity = new ArrayList<>();
    }

    public void setRouteBeanList(List<PoiInfoEntity> routeBeanList, List<RouteParam> loaclSaveEntity, int searchType) {
        if (null == routeBeanList) {
            return;
        }
        mRouteBeanList.clear();
        mRouteBeanList.addAll(routeBeanList);
        mLoaclSaveEntity.clear();
        mLoaclSaveEntity.addAll(loaclSaveEntity);
        mSearchType = searchType;
        notifyDataSetChanged();
    }

    public void setItemClickListener(OnItemClickListener itemClickListener) {
        this.itemClickListener = itemClickListener;
    }

    @Override
    public Holder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        RouteSearchChargeRefreshListItemBinding routeItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.route_search_charge_refresh_list_item, parent, false);
        return new Holder(routeItemBinding);
    }

    @Override
    public int getItemCount() {
        if (mRouteBeanList == null) {
            return 0;
        }
        return mRouteBeanList.size();
    }

    @Override
    public void onBindViewHolder(@NonNull Holder holder, int position) {
        holder.routeSearchChargeRefreshListItemBinding.itemRootViewCharge.setVisibility(ViewGroup.VISIBLE);
        holder.routeSearchChargeRefreshListItemBinding.routeItemChargeNum.setText("" + (position + NumberUtils.NUM_1));
        holder.routeSearchChargeRefreshListItemBinding.routeItemChargeName.setText(mRouteBeanList.get(position).getName());
        holder.routeSearchChargeRefreshListItemBinding.routeItemChargeDescription.setText(mRouteBeanList.get(position).getDistance() + " | " + mRouteBeanList.get(position).getAddress());
        boolean belongRouteParam;
        if (mSearchType == 0) {
            belongRouteParam = isBelongSamePoi(mLoaclSaveEntity, mRouteBeanList.get(position));
        } else {
            belongRouteParam = RoutePackage.getInstance().isBelongRouteParam(MapTypeId.MAIN_SCREEN_MAIN_MAP, mRouteBeanList.get(position));
        }
        holder.routeSearchChargeRefreshListItemBinding.routeItemChargeAddText.setText(belongRouteParam ? ResourceUtils.Companion.getInstance().getText(R.string.route_service_list_item_added) : ResourceUtils.Companion.getInstance().getText(R.string.route_service_list_item_add));
        holder.routeSearchChargeRefreshListItemBinding.routeItemChargeAddImg.setImageDrawable(belongRouteParam ? ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_search_added) : ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_search_add));
        holder.routeSearchChargeRefreshListItemBinding.routeItemChargeFastNumOne.setText(mRouteBeanList.get(position).getChargeInfoList().get(0).getFast_free() + "");
        if (mRouteBeanList.get(position).getChargeInfoList() != null && mRouteBeanList.get(position).getChargeInfoList().size() != 0) {
            holder.routeSearchChargeRefreshListItemBinding.routeItemChargeFastNumTwo.setText("/" + mRouteBeanList.get(position).getChargeInfoList().get(0).getFast_total() + "");
            holder.routeSearchChargeRefreshListItemBinding.routeItemChargeLowNumOne.setText(mRouteBeanList.get(position).getChargeInfoList().get(0).getSlow_free() + "");
            holder.routeSearchChargeRefreshListItemBinding.routeItemChargeLowNumTwo.setText("/" + mRouteBeanList.get(position).getChargeInfoList().get(0).getSlow_total() + "");
            if (!ConvertUtils.isEmpty(mRouteBeanList.get(position).getChargeInfoList().get(0).getCurrentElePrice())) {
                holder.routeSearchChargeRefreshListItemBinding.routeItemChargePriceName.setVisibility(View.VISIBLE);
                holder.routeSearchChargeRefreshListItemBinding.routeItemChargePriceNum.setVisibility(View.VISIBLE);
                holder.routeSearchChargeRefreshListItemBinding.routeItemChargePriceNum.setText(mRouteBeanList.get(position).getChargeInfoList().get(0).getCurrentElePrice() + "/度");
            } else {
                holder.routeSearchChargeRefreshListItemBinding.routeItemChargePriceNum.setVisibility(View.GONE);
                holder.routeSearchChargeRefreshListItemBinding.routeItemChargePriceName.setVisibility(View.GONE);
            }
        }
        holder.routeSearchChargeRefreshListItemBinding.itemRootViewCharge.setOnClickListener(v -> {
            if (itemClickListener != null) {
                itemClickListener.onItemClick(mRouteBeanList.get(position));
            }
        });

        boolean finalBelongRouteParam = belongRouteParam;
        holder.routeSearchChargeRefreshListItemBinding.routeItemChargeAddBg.setOnClickListener(v -> {
            if (itemClickListener != null) {
                if (finalBelongRouteParam) {
                    itemClickListener.onItermRemoveClick(mRouteBeanList.get(position));
                } else {
                    itemClickListener.onItermAddClick(mRouteBeanList.get(position));
                }

            }
        });
    }

    private boolean isBelongSamePoi(List<RouteParam> mLoaclSaveEntity, PoiInfoEntity poiInfoEntity) {
        if (mLoaclSaveEntity.isEmpty()) {
            return false;
        }
        for (RouteParam param: mLoaclSaveEntity) {
            if (RoutePackage.getInstance().isTheSamePoi(param, poiInfoEntity)) {
                return true;
            }
        }
        return false;
    }

    public class Holder extends RecyclerView.ViewHolder {
        public RouteSearchChargeRefreshListItemBinding routeSearchChargeRefreshListItemBinding;

        public Holder(RouteSearchChargeRefreshListItemBinding routeSearchChargeRefreshListItemBinding) {
            super(routeSearchChargeRefreshListItemBinding.getRoot());
            this.routeSearchChargeRefreshListItemBinding = routeSearchChargeRefreshListItemBinding;
            routeSearchChargeRefreshListItemBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        void onItemClick(PoiInfoEntity poiInfoEntity);
        void onItermRemoveClick(PoiInfoEntity poiInfoEntity);
        void onItermAddClick(PoiInfoEntity poiInfoEntity);
    }
}
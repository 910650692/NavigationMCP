package com.sgm.navi.scene.ui.adapter;

import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.RoutePoiIconItemBinding;
import com.sgm.navi.service.define.search.ServiceAreaInfo;

import java.util.ArrayList;
import java.util.List;

public class RoutePOIIconAdapter extends RecyclerView.Adapter<RoutePOIIconAdapter.Holder> {
    private List<ServiceAreaInfo.ServiceAreaChild> mRouteBeanList;
    private OnItemClickListener mItemClickListener;

    public RoutePOIIconAdapter() {
        mRouteBeanList = new ArrayList<>();
    }

    /***
     * 设置数据
     * @param routeBeanList 数据列表
     */
    public void setRouteBeanList(final List<ServiceAreaInfo.ServiceAreaChild> routeBeanList) {
        if (null == routeBeanList) {
            return;
        }

        mRouteBeanList.clear();
        mRouteBeanList.addAll(routeBeanList);
        notifyDataSetChanged();
    }

    /***
     * 设置监听
     * @param itemClickListener 点击监听
     */
    public void setItemClickListener(final OnItemClickListener itemClickListener) {
        this.mItemClickListener = itemClickListener;
    }

    @Override
    public Holder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final RoutePoiIconItemBinding routeItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.route_poi_icon_item, parent, false);
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
    public void onBindViewHolder(@NonNull final Holder holder, final int position) {
        switch (mRouteBeanList.get(position).getTypeCode()) {
            case "010200":
                holder.mRoutePoiIconItemBinding.poiServiceAreaImgLighting.setImageResource(R.drawable.img_route_poi_details_gas);
                break;
            case "200300":
                holder.mRoutePoiIconItemBinding.poiServiceAreaImgLighting.setImageResource(R.drawable.img_route_poi_details_wc);
                break;
            case "060400":
                holder.mRoutePoiIconItemBinding.poiServiceAreaImgLighting.setImageResource(R.drawable.img_route_poi_details_shopping);
                break;
            case "150904":
                holder.mRoutePoiIconItemBinding.poiServiceAreaImgLighting.setImageResource(R.drawable.img_route_poi_details_parking);
                break;
            case "011100":
                holder.mRoutePoiIconItemBinding.poiServiceAreaImgLighting.setImageResource(R.drawable.img_route_poi_details_charging);
                break;
            case "010000":
                holder.mRoutePoiIconItemBinding.poiServiceAreaImgLighting.setImageResource(R.drawable.img_route_poi_details_car_service);
                break;
            case "050100":
                holder.mRoutePoiIconItemBinding.poiServiceAreaImgLighting.setImageResource(R.drawable.img_route_poi_details_food);
                break;
            default:
                break;
        }
    }

    public class Holder extends RecyclerView.ViewHolder {
        private RoutePoiIconItemBinding mRoutePoiIconItemBinding;

        public Holder(final RoutePoiIconItemBinding routePoiIconItemBinding) {
            super(routePoiIconItemBinding.getRoot());
            this.mRoutePoiIconItemBinding = routePoiIconItemBinding;
            routePoiIconItemBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        /***
         * 点击监听
         * @param index 下标
         * @param isSelectIndex 选中下标
         */
        void onItemClick(int index, boolean isSelectIndex);
    }
}
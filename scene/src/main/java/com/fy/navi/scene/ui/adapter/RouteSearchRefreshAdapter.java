package com.fy.navi.scene.ui.adapter;

import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ResourceUtils;
import com.android.utils.TimeUtils;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.RouteSearchRefreshListItemBinding;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.route.RouteRestAreaDetailsInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.service.logicpaket.route.RoutePackage;

import java.util.ArrayList;
import java.util.List;

public class RouteSearchRefreshAdapter extends RecyclerView.Adapter<RouteSearchRefreshAdapter.Holder> {
    private List<RouteRestAreaDetailsInfo> mRouteBeanList;
    private OnItemClickListener mItemClickListener;
    public RouteSearchRefreshAdapter() {
        mRouteBeanList = new ArrayList<>();
    }

    /***
     * 设置数据
     * @param routeBeanList 数据列表
     */
    public void setRouteBeanList(final List<RouteRestAreaDetailsInfo> routeBeanList) {
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
        final RouteSearchRefreshListItemBinding routeItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.route_search_refresh_list_item, parent, false);
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
        holder.mRouteSearchRefreshListItemBinding.routeItemServiceNum.setText("" + (position + NumberUtils.NUM_1));
        holder.mRouteSearchRefreshListItemBinding.routeItemServiceName.setText(mRouteBeanList.get(position).getMServiceName());
        holder.mRouteSearchRefreshListItemBinding.routeItemServiceDescription.setText(TimeUtils.getInstance()
                .getDistanceString(mRouteBeanList.get(position).getMRemainDist()));
        final boolean belongRouteParam = RoutePackage.getInstance().isBelongRouteParam(MapType.MAIN_SCREEN_MAIN_MAP
                , getPoiEntry(mRouteBeanList.get(position)));
        holder.mRouteSearchRefreshListItemBinding.routeItemServiceAddText.setText(belongRouteParam
                ? ResourceUtils.Companion.getInstance().getText(R.string.route_service_list_item_added)
                : ResourceUtils.Companion.getInstance().getText(R.string.route_service_list_item_add));
        holder.mRouteSearchRefreshListItemBinding.routeItemServiceAddImg.setImageDrawable(belongRouteParam
                ? ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_search_added)
                : ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_search_add));
        holder.mRouteSearchRefreshListItemBinding.itemRootViewService.setOnClickListener(v -> {
            if (mItemClickListener != null) {
                mItemClickListener.onItemClick(getPoiEntry(mRouteBeanList.get(position)));
            }
        });

        holder.mRouteSearchRefreshListItemBinding.routeItemServiceAddBg.setOnClickListener(v -> {
            if (mItemClickListener != null) {
                mItemClickListener.onItermAddClick(getPoiEntry(mRouteBeanList.get(position)));
            }
        });
    }

    /***
     * 转换点信息
     * @param info 详情信息
     * @return 点信息
     */
    private PoiInfoEntity getPoiEntry(final RouteRestAreaDetailsInfo info) {
        final PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
        poiInfoEntity.setName(info.getMServiceName());
        poiInfoEntity.setPid(info.getMServicePOIID());
        poiInfoEntity.setPoint(new GeoPoint(info.getMPos().getLon(), info.getMPos().getLat()));
        poiInfoEntity.setPointTypeCode("1083");
        poiInfoEntity.setPoiTag("服务区");
        poiInfoEntity.setDistance(TimeUtils.getInstance().getDistanceMsg(info.getMRemainDist()));
        poiInfoEntity.setPoint(new GeoPoint(info.getMPos().getLon(), info.getMPos().getLat()));
        return poiInfoEntity;
    }

    public class Holder extends RecyclerView.ViewHolder {
        private RouteSearchRefreshListItemBinding mRouteSearchRefreshListItemBinding;

        public Holder(final RouteSearchRefreshListItemBinding routeSearchRefreshListItemBinding) {
            super(routeSearchRefreshListItemBinding.getRoot());
            this.mRouteSearchRefreshListItemBinding = routeSearchRefreshListItemBinding;
            routeSearchRefreshListItemBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        /***
         * 点击监听
         * @param poiInfoEntity 点信息
         */
        void onItemClick(PoiInfoEntity poiInfoEntity);

        /***
         * 点击添加按钮
         * @param poiInfoEntity 点信息
         */
        void onItermAddClick(PoiInfoEntity poiInfoEntity);
    }
}
package com.fy.navi.hmi.route.alternative;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.AppCompatTextView;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.hmi.R;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.route.RouteAlterChargeStationInfo;
import com.fy.navi.ui.view.SkinConstraintLayout;

import java.util.ArrayList;
import java.util.List;

/**
 * Author: LiuChang
 * Date: 2025/3/6
 * Description: [替换充电站适配器]
 */
public class AlterChargeStationAdapter extends RecyclerView.Adapter<AlterChargeStationAdapter.AlterChargeStationViewHolder> {
    private List<RouteAlterChargeStationInfo> data = new ArrayList<>();
    private ItemClickListener mListener;
    private Context mContext;

    public AlterChargeStationAdapter(Context context, List<RouteAlterChargeStationInfo> data) {
        this.mContext = context;
        this.data.clear();
        this.data = data;
    }

    public void setData(List<RouteAlterChargeStationInfo> data) {
        this.data.clear();
        this.data = data;
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public AlterChargeStationViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(mContext).inflate(R.layout.item_alter_charge_stations, parent, false);
        return new AlterChargeStationViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull AlterChargeStationViewHolder holder, int position) {
        holder.tvName.setText(data.get(position).name);
        holder.tvCost.setText(mContext.getString(R.string.route_charge_stations_cost,
                 data.get(position).priceInfo.lowestPriceValue, data.get(position).priceInfo.lowestPriceUnit));

        String fastTotalNumber = data.get(position).fastPlugInfo.totalNumber;
        if (fastTotalNumber.equals("")) {
            fastTotalNumber = mContext.getString(R.string.route_invalid);
        }
        holder.tvFast.setText(mContext.getString(R.string.route_charge_stations_fast, "-"
                , fastTotalNumber));

        String slowTotalNumber = data.get(position).slowPlugInfo.totalNumber;
        if (slowTotalNumber.equals("")) {
            slowTotalNumber = mContext.getString(R.string.route_invalid);
        }
        holder.tvSlow.setText(mContext.getString(R.string.route_charge_stations_slow, "-"
                , slowTotalNumber));
        holder.tvDistance.setText(mContext.getString(R.string.route_invalid));

        holder.layoutChargeStation.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (mListener != null) {
                    mListener.onItemClick(data.get(position).poiId);
                }
            }
        });

        holder.buttonAlter.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (mListener != null) {
                    mListener.onAlterClick(data.get(position));
                }
            }
        });
    }

    @Override
    public int getItemCount() {
        return data.size();
    }

    public static class AlterChargeStationViewHolder extends RecyclerView.ViewHolder {
        AppCompatTextView tvName;
        AppCompatTextView tvDistance;
        AppCompatTextView tvFast;
        AppCompatTextView tvSlow;
        AppCompatTextView tvCost;
        AppCompatTextView buttonAlter;
        SkinConstraintLayout layoutChargeStation;


        public AlterChargeStationViewHolder(@NonNull View itemView) {
            super(itemView);
            tvName = itemView.findViewById(R.id.tv_charge_station_name);
            tvDistance = itemView.findViewById(R.id.tv_charge_station_distance);
            tvFast = itemView.findViewById(R.id.tv_charge_station_fast);
            tvSlow = itemView.findViewById(R.id.tv_charge_station_slow);
            tvCost = itemView.findViewById(R.id.tv_charge_station_cost);
            buttonAlter = itemView.findViewById(R.id.button_alter);
            layoutChargeStation = itemView.findViewById(R.id.layout_charge_station);
        }
    }

    public void setListener(ItemClickListener listener) {
        mListener = listener;
    }

    public interface ItemClickListener {
        void onItemClick(String poiID);
        void onAlterClick(RouteAlterChargeStationInfo info);
    }
}